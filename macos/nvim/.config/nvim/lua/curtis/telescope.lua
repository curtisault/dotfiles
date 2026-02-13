local builtin = require('telescope.builtin')

------ Keymaps ------
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>gf', builtin.git_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})

vim.keymap.set('n', '<leader>rg', function()
	builtin.grep_string({ search = vim.fn.input("rg > ")});
end)

require('telescope').setup {
  defaults = {
    cache_picker = {
      num_pickers = 10,
    },
  }
}
