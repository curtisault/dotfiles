require('nvim-treesitter').setup()

-- Install parsers
local ensure_installed = {
  "c",
  "lua",
  "query",
  "vim",
  "vimdoc",
  "elixir",
  "erlang",
  "heex",
  "rust",
  "markdown",
  "markdown_inline",
  "sql",
}

local installed = require('nvim-treesitter').get_installed()
local to_install = vim.tbl_filter(function(parser)
  return not vim.tbl_contains(installed, parser)
end, ensure_installed)

if #to_install > 0 then
  require('nvim-treesitter').install(to_install)
end

-- Enable treesitter-based highlighting (built into Neovim)
vim.api.nvim_create_autocmd("FileType", {
  callback = function(args)
    local ok = pcall(vim.treesitter.start, args.buf)
    if not ok then
      return
    end
  end,
})
