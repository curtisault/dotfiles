-- orchestrator.lua — Parent context manager (Claude as coordinator).
--
-- Manages the top-level conversation with Claude that decomposes user requests,
-- integrates subagent results, and synthesizes the final output.
-- Supports recursive delegation: any agent can act as a local orchestrator
-- for its own subtasks.

local M = {}

local log     = require("agentflow.util.log")
local json    = require("agentflow.util.json")
local planner = require("agentflow.planner")
local router  = require("agentflow.router")
local events  = require("agentflow.util.events")

-- ── System prompt loader ──────────────────────────────────────────────────────

local _system_prompt_cache = nil

local function load_system_prompt(override)
  if override then return override end
  if _system_prompt_cache then return _system_prompt_cache end

  -- Try to read from the prompts/ file bundled with the plugin
  local plugin_dir = vim.fn.fnamemodify(
    vim.api.nvim_get_runtime_file("lua/agentflow/init.lua", false)[1] or "",
    ":h:h:h"  -- …/agentflow/
  )
  local prompt_path = plugin_dir .. "/prompts/orchestrator.md"

  local ok, lines = pcall(vim.fn.readfile, prompt_path)
  if ok and lines and #lines > 0 then
    _system_prompt_cache = table.concat(lines, "\n")
    log.debug("Loaded orchestrator system prompt", { path = prompt_path })
    return _system_prompt_cache
  end

  -- Minimal inline fallback
  _system_prompt_cache = table.concat({
    "You are AgentFlow's orchestrator. Decompose the user's request into tasks.",
    "Output a JSON plan inside a ```json code block with this structure:",
    '{ "tasks": [{ "id": "t1", "description": "...", "task_type": "analysis",',
    '  "context_requirements": { "files": [], "scope": "function", "include_git": false },',
    '  "depends_on": [], "agent_hint": null }] }',
    "Tasks with no depends_on run in parallel.",
  }, "\n")

  log.warn("orchestrator: using inline fallback system prompt (prompts/orchestrator.md not found)")
  return _system_prompt_cache
end

-- ── Constructor ───────────────────────────────────────────────────────────────

--- Create a new orchestrator instance.
--- @param cfg table  Full agentflow config (from config.get())
--- @return table Orchestrator
function M.new(cfg)
  local self = {
    cfg              = cfg,
    conversation     = {},   -- Full message history with the orchestrator model
    active_plan      = nil,  -- Current Plan object
    pending_results  = {},   -- agent_id → result

    -- Cost tracking across the entire tree
    _cost = { tokens_in = 0, tokens_out = 0, agent_count = 0 },

    -- Backend adapter (lazy)
    _backend = nil,

    -- Agent pool (lazy)
    _pool = nil,
  }
  return setmetatable(self, { __index = M })
end

-- ── Internal helpers ──────────────────────────────────────────────────────────

function M:_get_backend()
  if not self._backend then
    local backend_mod = require("agentflow.backend")
    self._backend = backend_mod.get(
      self.cfg.backend.primary or "cli",
      { cli_path = self.cfg.backend.cli_path, cli_flags = self.cfg.backend.cli_flags }
    )
  end
  return self._backend
end

function M:_get_pool()
  if not self._pool then
    local pool_mod = require("agentflow.agents.pool")
    self._pool = pool_mod.new(
      self.cfg.concurrency.max_parallel_agents,
      self.cfg.concurrency.max_total_agents
    )
  end
  return self._pool
end

function M:_track_cost(result)
  if not result then return end
  self._cost.tokens_in   = self._cost.tokens_in   + (result.tokens_in  or 0)
  self._cost.tokens_out  = self._cost.tokens_out  + (result.tokens_out or 0)
  self._cost.agent_count = self._cost.agent_count + 1
end

-- ── submit() ─────────────────────────────────────────────────────────────────

--- Send the user's message to the orchestrator model and get back a Plan.
--- Must be called from within a coroutine.
---
--- @param user_message string
--- @param context string|nil  Pre-built context string (from context/init.lua)
--- @param opts table|nil { on_token? function }
--- @return table|nil  Plan object from planner.parse()
--- @return string|nil error
function M:submit(user_message, context, opts)
  opts = opts or {}

  -- Append the user turn (include context as a preamble if provided)
  local content = user_message
  if context and context ~= "" then
    content = "## Context\n\n" .. context .. "\n\n## Request\n\n" .. user_message
  end
  table.insert(self.conversation, { role = "user", content = content })

  local system = load_system_prompt(
    self.cfg.orchestrator and self.cfg.orchestrator.system_prompt
  )

  local adapter = self:_get_backend()
  local result, err = adapter:complete(self.conversation, {
    model      = self.cfg.orchestrator.model,
    max_tokens = 4096,
    system     = system,
    on_token   = opts.on_token,
  })

  if err then
    log.error("orchestrator: backend error", { error = err })
    return nil, err
  end

  self:_track_cost(result)

  -- Append assistant turn
  table.insert(self.conversation, { role = "assistant", content = result.content })

  log.debug("orchestrator: got response", { chars = #result.content })

  -- Parse the plan
  local plan, parse_err = planner.parse(result.content)
  if not plan then
    -- Retry once: ask Claude to fix its JSON
    log.warn("orchestrator: plan parse failed, requesting repair", { error = parse_err })

    local repair_msg = "Your previous response could not be parsed as a task plan. " ..
      "Error: " .. parse_err .. "\n\n" ..
      "Please output ONLY a valid JSON plan block and nothing else."
    table.insert(self.conversation, { role = "user", content = repair_msg })

    local retry, retry_err = adapter:complete(self.conversation, {
      model      = self.cfg.orchestrator.model,
      max_tokens = 4096,
      system     = system,
    })

    if retry_err then return nil, retry_err end
    self:_track_cost(retry)
    table.insert(self.conversation, { role = "assistant", content = retry.content })

    plan, parse_err = planner.parse(retry.content)
    if not plan then
      return nil, "orchestrator: plan parse failed after retry — " .. (parse_err or "?")
    end
  end

  self.active_plan = plan
  events.emit("plan:created", { plan = plan })
  log.info("orchestrator: plan created", {
    tasks  = #plan.tasks,
    groups = #plan.execution_order,
  })

  return plan, nil
end

-- ── run_plan() ────────────────────────────────────────────────────────────────

--- Execute all tasks in the active plan using the pool.
--- Must be called from within a coroutine.
---
--- @param plan table        Plan from submit()
--- @param opts table|nil  { on_token? function, context_override? string }
--- @return table[]  All task results { task_id → { ok, result|error } }
function M:run_plan(plan, opts)
  opts = opts or {}

  local registry  = require("agentflow.agents")
  local agent_mod = require("agentflow.agents.agent")
  local context_mod = require("agentflow.context")
  local pool = self:_get_pool()

  local all_results = {}  -- task.id → { ok, result, error }

  -- Execute each parallel group in order
  for group_idx, group in ipairs(plan.execution_order) do
    log.info("orchestrator: executing group", { group = group_idx, tasks = group })

    -- Build submissions for this group
    local submissions = {}
    for _, task_id in ipairs(group) do
      local task = planner.get_task(plan, task_id)
      if not task then
        log.warn("orchestrator: task not found in plan", { id = task_id })
      else
        -- Route to an agent
        local agent_cfg_name = router.assign(
          task,
          registry.list(),
          self.cfg.routing.rules or {},
          nil,  -- all available (pool handles concurrency)
          { filetype = vim.api.nvim_get_option_value("filetype", { buf = 0 }) }
        )

        if not agent_cfg_name then
          log.error("orchestrator: no agent for task", { task = task_id })
          all_results[task_id] = { ok = false, error = "no agent assigned" }
        else
          local agent_cfg = registry.get(agent_cfg_name)
          local agent = agent_mod.new(agent_cfg, { events = events })

          -- Build context for this task
          local context = opts.context_override
          if not context then
            local ok_ctx, ctx_or_err = pcall(context_mod.build, task, self.cfg)
            context = ok_ctx and ctx_or_err or ""
            if not ok_ctx then
              log.warn("orchestrator: context build failed", { err = ctx_or_err })
            end
          end

          task.status = "running"
          events.emit("agent:assigned", { agent = agent, task = task })

          table.insert(submissions, {
            agent   = agent,
            task    = task,
            context = context,
            opts    = { on_token = opts.on_token },
          })
        end
      end
    end

    -- Run this group in parallel and wait
    local group_results = pool:submit_group(submissions)

    for _, res in ipairs(group_results) do
      local task_id = res.task and res.task.id
      if task_id then
        all_results[task_id] = res
        local task = planner.get_task(plan, task_id)
        if task then
          task.status = res.ok and "done" or "failed"
          task.result = res.result
        end
        -- Feed result back into orchestrator conversation
        self:integrate_result(task_id, res)
        self:_track_cost(res.result)
      end
    end
  end

  return all_results
end

-- ── integrate_result() ────────────────────────────────────────────────────────

--- Feed a subagent result back into the orchestrator conversation.
--- Must be called from within a coroutine (it may trigger dependent tasks).
---
--- @param agent_id string  Task ID
--- @param res table        { ok, result, error, task }
function M:integrate_result(agent_id, res)
  local status  = res.ok and "COMPLETED" or "FAILED"
  local content = res.ok
    and (res.result and res.result.content or "(empty result)")
    or  ("Error: " .. (res.error or "unknown"))

  local msg = string.format(
    "[Subagent result — task %s — %s]\n\n%s",
    agent_id, status, content
  )

  table.insert(self.conversation, { role = "user", content = msg })
  self.pending_results[agent_id] = res

  log.debug("orchestrator: integrated result", { task = agent_id, ok = res.ok })
end

-- ── synthesize() ─────────────────────────────────────────────────────────────

--- Ask the orchestrator to synthesize all subagent results into a final response.
--- Must be called from within a coroutine.
---
--- @param opts table|nil { on_token? function }
--- @return table|nil  { content, tokens_in, tokens_out, model }
--- @return string|nil error
function M:synthesize(opts)
  opts = opts or {}

  table.insert(self.conversation, {
    role    = "user",
    content = "All subagents have completed. Please synthesize their results into a " ..
              "coherent final response for the user. Describe what changes should be " ..
              "applied and in what order. Flag anything requiring user judgement.",
  })

  local system  = load_system_prompt(self.cfg.orchestrator and self.cfg.orchestrator.system_prompt)
  local adapter = self:_get_backend()

  local result, err = adapter:complete(self.conversation, {
    model      = self.cfg.orchestrator.model,
    max_tokens = 8192,
    system     = system,
    on_token   = opts.on_token,
  })

  if err then
    log.error("orchestrator: synthesize error", { error = err })
    return nil, err
  end

  self:_track_cost(result)
  table.insert(self.conversation, { role = "assistant", content = result.content })

  events.emit("orchestrator:synthesized", { result = result, cost = self._cost })
  log.info("orchestrator: synthesis complete", {
    tokens_in  = result.tokens_in,
    tokens_out = result.tokens_out,
  })

  return result, nil
end

-- ── High-level workflow ───────────────────────────────────────────────────────

--- Full workflow: submit → run_plan → synthesize.
--- Must be called from within a coroutine.
---
--- @param prompt string
--- @param opts table|nil { on_token?, on_plan?, on_complete? }
--- @return table|nil  Final synthesis result
--- @return string|nil error
function M:run(prompt, opts)
  opts = opts or {}

  -- Build initial context
  local context_mod = require("agentflow.context")
  local context = ""
  local ok_ctx, ctx = pcall(context_mod.build_simple, self.cfg)
  if ok_ctx then context = ctx end

  -- Decompose
  local plan, err = self:submit(prompt, context, { on_token = opts.on_token })
  if not plan then return nil, err end
  if opts.on_plan then pcall(opts.on_plan, plan) end

  -- Execute
  self:run_plan(plan, { on_token = opts.on_token })

  -- Synthesize
  local final, syn_err = self:synthesize({ on_token = opts.on_token })
  if syn_err then return nil, syn_err end

  if opts.on_complete then pcall(opts.on_complete, final) end
  return final, nil
end

-- ── Accessors ─────────────────────────────────────────────────────────────────

function M:get_conversation() return self.conversation end
function M:get_plan()         return self.active_plan   end
function M:get_cost()         return vim.deepcopy(self._cost) end

--- Reset orchestrator state for a fresh workflow.
function M:reset()
  self.conversation    = {}
  self.active_plan     = nil
  self.pending_results = {}
  self._cost           = { tokens_in = 0, tokens_out = 0, agent_count = 0 }
  self._pool           = nil
  log.debug("orchestrator: reset")
end

return M
