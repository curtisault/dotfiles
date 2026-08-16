vim.opt.nu = true vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8

vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.ignorecase = true

vim.opt.clipboard = 'unnamedplus'

vim.opt.cursorline = true
vim.opt.swapfile = false
vim.opt.updatetime = 1000


-- colorschemes

-- ------ Gruvbox ------
-- vim.cmd[[colorscheme gruvbox]]
-- vim.opt.background = 'dark'
-- vim.opt.background = 'light'

-- ------ Everforest ------
-- neanias/everforest-nvim (lua port) — configured via setup(), not the
-- vim.g.everforest_* globals used by the original vimscript plugin.
-- background: 'soft' | 'medium' | 'hard'
-- setup() + vim.opt.background must both precede the colorscheme command,
-- since the palette is generated from vim.o.background at load time.
-- require('everforest').setup({ background = 'soft' })
-- vim.opt.background = 'light'
-- vim.cmd [[colorscheme everforest]]

-- ------ Catppuccin ------
-- vim.cmd [[colorscheme catppuccin]]
-- vim.cmd [[colorscheme catppuccin-latte]]
-- vim.cmd [[colorscheme catppuccin-frappe]]
-- vim.cmd [[colorscheme catppuccin-macchiato]]
-- vim.cmd [[colorscheme catppuccin-mocha]]

-- ------ Zenbones ------
-- compat mode = no lush.nvim dependency required
-- (must be g:bones_compat for all variants; g:zenbones_compat only covers `zenbones`)
-- variants: zenbones / zenwritten / neobones / vimbones / rosebones
--           forestbones / nordbones / tokyobones / seoulbones / duckbones
--           zenburned / kanagawabones / randombones
vim.g.bones_compat = 1
vim.opt.background = 'light'
-- vim.opt.background = 'dark'
vim.cmd [[colorscheme forestbones]]

