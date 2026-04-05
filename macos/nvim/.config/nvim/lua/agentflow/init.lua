-- agentflow/init.lua — Plugin entry point
-- Usage: require("agentflow").setup(opts)

local M = {}

local log = require("agentflow.util.log")
local config = require("agentflow.config")

-- Singleton state
local _initialized = false

-- ── Setup ────────────────────────────────────────────────────────────────────

--- Main setup function. Call once from your Neovim config.
--- @param opts table|nil  User config (merged with defaults)
function M.setup(opts)
  if _initialized then
    log.warn("agentflow.setup() called more than once — ignoring")
    return
  end

  -- Merge and validate config
  local cfg, errors = config.setup(opts or {})
  if errors and #errors > 0 then
    for _, err in ipairs(errors) do
      vim.notify("AgentFlow config error: " .. err, vim.log.levels.ERROR)
    end
    return
  end

  -- Boot logger with user preferences
  log.setup(cfg.log or {})
  log.info("AgentFlow initializing")

  -- Register commands
  M._register_commands()

  -- Register keymaps
  M._register_keymaps()

  _initialized = true
  log.info("AgentFlow ready")
end

-- ── Commands ─────────────────────────────────────────────────────────────────

function M._register_commands()
  -- :AgentFlow            → open hub
  -- :AgentFlow {prompt}   → start workflow with prompt
  vim.api.nvim_create_user_command("AgentFlow", function(cmd_opts)
    if cmd_opts.args and cmd_opts.args ~= "" then
      M.start(cmd_opts.args)
    else
      M.hub()
    end
  end, {
    nargs = "?",
    desc = "Open AgentFlow hub or start a workflow with a prompt",
  })

  vim.api.nvim_create_user_command("AgentChat", function()
    M.chat()
  end, { desc = "Open AgentFlow chat pane" })

  vim.api.nvim_create_user_command("AgentTree", function()
    M.tree()
  end, { desc = "Open AgentFlow agent tree view" })

  vim.api.nvim_create_user_command("AgentDash", function()
    M.dashboard()
  end, { desc = "Open AgentFlow dashboard view" })

  vim.api.nvim_create_user_command("AgentGrid", function()
    M.grid()
  end, { desc = "Open AgentFlow grid view" })

  vim.api.nvim_create_user_command("AgentConfig", function()
    M.config_panel()
  end, { desc = "Open AgentFlow configuration panel" })

  vim.api.nvim_create_user_command("AgentReview", function()
    M.review()
  end, { desc = "Open AgentFlow review panel" })

  vim.api.nvim_create_user_command("AgentLog", function(cmd_opts)
    if cmd_opts.args and cmd_opts.args ~= "" then
      log.info("Showing log for agent: " .. cmd_opts.args)
    end
    log.open()
  end, {
    nargs = "?",
    desc = "Show AgentFlow log buffer (optionally filter by agent name)",
  })

  vim.api.nvim_create_user_command("AgentKill", function(cmd_opts)
    M.kill(cmd_opts.args)
  end, {
    nargs = 1,
    desc = "Terminate an agent and its subtree",
  })

  vim.api.nvim_create_user_command("AgentAdd", function(cmd_opts)
    -- args: "{name} {model}"
    local parts = vim.split(cmd_opts.args, "%s+", { trimempty = true })
    if #parts < 2 then
      vim.notify("Usage: :AgentAdd {name} {model}", vim.log.levels.ERROR)
      return
    end
    M.add_agent(parts[1], parts[2])
  end, {
    nargs = "+",
    desc = "Register a new agent by name and model",
  })

  vim.api.nvim_create_user_command("AgentPick", function()
    M.pick_agent()
  end, { desc = "Open picker to select an agent" })
end

-- ── Keymaps ──────────────────────────────────────────────────────────────────

function M._register_keymaps()
  local cfg = config.get()
  if not cfg.keymaps or cfg.keymaps.enabled == false then return end

  local map = function(lhs, rhs, desc)
    vim.keymap.set("n", lhs, rhs, { desc = desc, silent = true })
  end

  map("<leader>af", M.hub,         "AgentFlow: open hub")
  map("<leader>ap", function()
    vim.ui.input({ prompt = "AgentFlow prompt: " }, function(input)
      if input and input ~= "" then M.start(input) end
    end)
  end, "AgentFlow: start with prompt")
  map("<leader>as", M.tree,        "AgentFlow: agent tree")
  map("<leader>ac", M.config_panel,"AgentFlow: config panel")
  map("<leader>ar", M.review,      "AgentFlow: review panel")
end

-- ── Public actions ────────────────────────────────────────────────────────────
-- These are stubs that will be wired to real implementations in later phases.

function M.start(prompt)
  log.info("Starting workflow", { prompt = prompt })
  -- Phase 4: will call orchestrator:submit(prompt)
  vim.notify("AgentFlow: workflow starting… (orchestrator not yet wired)", vim.log.levels.INFO)
end

function M.hub()
  log.debug("Opening hub")
  -- Phase 5: will open ui/hub.lua
  vim.notify("AgentFlow: hub (UI not yet built)", vim.log.levels.INFO)
end

function M.chat()
  log.debug("Opening chat")
  local ok, chat_ui = pcall(require, "agentflow.ui.chat")
  if ok then
    chat_ui.open()
  else
    vim.notify("AgentFlow: chat UI not yet built", vim.log.levels.INFO)
  end
end

function M.tree()
  log.debug("Opening tree view")
  vim.notify("AgentFlow: tree view (UI not yet built)", vim.log.levels.INFO)
end

function M.dashboard()
  log.debug("Opening dashboard")
  vim.notify("AgentFlow: dashboard (UI not yet built)", vim.log.levels.INFO)
end

function M.grid()
  log.debug("Opening grid view")
  vim.notify("AgentFlow: grid view (UI not yet built)", vim.log.levels.INFO)
end

function M.config_panel()
  log.debug("Opening config panel")
  vim.notify("AgentFlow: config panel (UI not yet built)", vim.log.levels.INFO)
end

function M.review()
  log.debug("Opening review panel")
  vim.notify("AgentFlow: review panel (UI not yet built)", vim.log.levels.INFO)
end

function M.status()
  log.info("Status requested")
  vim.notify("AgentFlow: status (not yet implemented)", vim.log.levels.INFO)
end

function M.cancel()
  log.warn("Cancel requested")
  vim.notify("AgentFlow: cancel (not yet implemented)", vim.log.levels.WARN)
end

function M.kill(agent_name)
  log.warn("Kill requested", { agent = agent_name })
  vim.notify("AgentFlow: kill " .. agent_name .. " (not yet implemented)", vim.log.levels.WARN)
end

function M.add_agent(name, model)
  log.info("Add agent requested", { name = name, model = model })
  vim.notify("AgentFlow: add agent (registry not yet built)", vim.log.levels.INFO)
end

function M.pick_agent()
  log.debug("Pick agent requested")
  vim.notify("AgentFlow: picker (UI not yet built)", vim.log.levels.INFO)
end

return M
