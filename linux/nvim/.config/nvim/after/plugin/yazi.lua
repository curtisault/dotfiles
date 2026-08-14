-- yazi in a floating window.
-- --chooser-file makes yazi write the selected path(s) there and exit, then we
-- open them. Avoids nesting nvim inside nvim.

local function open(path, opencmd)
  if vim.fn.executable("yazi") == 0 then
    vim.notify("yazi not found on PATH", vim.log.levels.ERROR)
    return
  end

  local chooser = vim.fn.tempname()
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * 0.9)
  local height = math.floor(vim.o.lines * 0.9)

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    style = "minimal",
    border = "rounded",
  })

  vim.fn.jobstart({ "yazi", "--chooser-file", chooser, path }, {
    term = true,
    on_exit = function()
      if vim.api.nvim_win_is_valid(win) then
        vim.api.nvim_win_close(win, true)
      end
      if vim.api.nvim_buf_is_valid(buf) then
        vim.api.nvim_buf_delete(buf, { force = true })
      end

      local chosen = {}
      if vim.fn.filereadable(chooser) == 1 then
        chosen = vim.fn.readfile(chooser)
      end
      vim.fn.delete(chooser)

      -- With tabedit, a multi-file selection lands one file per tab.
      for _, file in ipairs(chosen) do
        if file ~= "" then
          vim.cmd[opencmd](vim.fn.fnameescape(file))
        end
      end
    end,
  })

  vim.cmd.startinsert()
end

-- No argument opens at the current file's directory.
local function define(name, opencmd)
  vim.api.nvim_create_user_command(name, function(opts)
    local path = opts.args
    if path == "" then
      local dir = vim.fn.expand("%:p:h")
      path = dir ~= "" and dir or vim.fn.getcwd()
    end
    open(path, opencmd)
  end, { nargs = "?", complete = "dir", desc = "Open yazi (" .. opencmd .. ")" })
end

define("Yazi", "edit")
define("YaziTab", "tabedit")
