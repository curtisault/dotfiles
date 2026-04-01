vim.g.mapleader = " "

------------- Normal -------------
vim.keymap.set('n', '<esc>', '<cmd>noh<CR>') -- Clear highlights


------------- Diagnostics -------------
vim.keymap.set('n', '<leader>dt', '<cmd>Telescope diagnostics<CR>')


-- Window Split
vim.keymap.set('n', '<leader>wl', '<cmd>vsplit<CR>')
vim.keymap.set('n', '<leader>wj', '<cmd>split<CR>')
vim.keymap.set('n', '<leader>wd', '<cmd>close<CR>')


-- Window Nav
vim.keymap.set('n', '<C-h>', '<C-w>h<CR>')
vim.keymap.set('n', '<C-j>', '<C-w>j<CR>')
vim.keymap.set('n', '<C-k>', '<C-w>k<CR>')
vim.keymap.set('n', '<C-l>', '<C-w>l<CR>')


-- Window Resize
vim.keymap.set('n', '<S-Up>', '<cmd>resize -2<CR>')
vim.keymap.set('n', '<S-Down>', '<cmd>resize +2<CR>')
vim.keymap.set('n', '<S-Left>', '<cmd>vertical resize -2<CR>')
vim.keymap.set('n', '<S-Right>', '<cmd>vertical resize +2<CR>')


-- Tab
vim.keymap.set('n', 'tt', '<c-w>T')
vim.keymap.set('n', 'th', ':tabfirst<CR>')
vim.keymap.set('n', 'tj', ':tabprev<CR>')
vim.keymap.set('n', 'tk', ':tabnext<CR>')
vim.keymap.set('n', 'tl', ':tablast<CR>')
vim.keymap.set('n', 'tn', ':tabnew<Space>')
vim.keymap.set('n', 'td', ':tabclose<CR>')


------------- Visual -------------
vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')


------ Claude Code ------
vim.keymap.set('n', '<leader>ac', '<cmd>ClaudeCode<cr>', { desc = 'Toggle Claude' })
vim.keymap.set('n', '<leader>af', '<cmd>ClaudeCodeFocus<cr>', { desc = 'Focus Claude' })
vim.keymap.set('n', '<leader>ar', '<cmd>ClaudeCode --resume<cr>', { desc = 'Resume Claude' })
vim.keymap.set('n', '<leader>aC', '<cmd>ClaudeCode --continue<cr>', { desc = 'Continue Claude' })
vim.keymap.set('n', '<leader>am', '<cmd>ClaudeCodeSelectModel<cr>', { desc = 'Select Claude model' })
vim.keymap.set('n', '<leader>ab', '<cmd>ClaudeCodeAdd %<cr>', { desc = 'Add current buffer' })
vim.keymap.set('v', '<leader>as', '<cmd>ClaudeCodeSend<cr>', { desc = 'Send to Claude' })
vim.keymap.set('n', '<leader>aa', '<cmd>ClaudeCodeDiffAccept<cr>', { desc = 'Accept diff' })
vim.keymap.set('n', '<leader>ad', '<cmd>ClaudeCodeDiffDeny<cr>', { desc = 'Deny diff' })

vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'NvimTree', 'neo-tree', 'oil' },
  callback = function()
    vim.keymap.set('n', '<leader>as', '<cmd>ClaudeCodeTreeAdd<cr>', { desc = 'Add file', buffer = true })
  end,
})


------ Sessions ------
vim.keymap.set('n', '<leader>sw', '<cmd>lua MiniSessions.write(vim.fn.input("Session name: "))<CR>', { desc = 'Write session' })
vim.keymap.set('n', '<leader>sr', '<cmd>lua MiniSessions.select()<CR>', { desc = 'Restore session' })
vim.keymap.set('n', '<leader>sd', '<cmd>lua MiniSessions.select("delete")<CR>', { desc = 'Delete session' })


------ Fuzzy ------
vim.keymap.set('n', '<leader>ff', '<cmd>Pick files<CR>')
vim.keymap.set('n', '<leader>fg', '<cmd>Pick grep_live<CR>')
vim.keymap.set('n', '<leader>fb', '<cmd>Pick buffers<CR>')

