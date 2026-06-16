vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup({
  sort = { sorter = "case_sensitive" },
  view = { width = 35 },
  renderer = { group_empty = true },
  filters = { dotfiles = false },
  git = { enable = true },
  actions = {
    open_file = {
      quit_on_open = false,
    },
  },
})
