vim.pack.add({
  -- mini suite
  'https://github.com/echasnovski/mini.nvim',

  -- Color schemes
  'https://github.com/neanias/everforest-nvim',
  { src = 'https://github.com/catppuccin/nvim', name = 'catppuccin' },
  'https://github.com/ellisonleao/gruvbox.nvim',

  -- Icons (used by oil, lualine, render-markdown)
  'https://github.com/nvim-tree/nvim-web-devicons',

  -- File explorer
  'https://github.com/stevearc/oil.nvim',

  -- Git
  'https://github.com/tpope/vim-dispatch',
  'https://github.com/tpope/vim-fugitive',

  -- LSP
  'https://github.com/neovim/nvim-lspconfig',
  'https://github.com/williamboman/mason.nvim',
  'https://github.com/williamboman/mason-lspconfig.nvim',

  -- Treesitter
  'https://github.com/nvim-treesitter/nvim-treesitter',

  -- Statusline
  'https://github.com/nvim-lualine/lualine.nvim',

  -- Markdown rendering
  'https://github.com/MeanderingProgrammer/render-markdown.nvim',

  -- Claude Code
  'https://github.com/folke/snacks.nvim',
  'https://github.com/coder/claudecode.nvim',

  -- AgentFlow
  -- 'https://github.com/curtisault/agentflow.nvim',
})

-- Run TSUpdate after treesitter install/update
vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    if ev.data.spec.name == 'nvim-treesitter' and
       (ev.data.kind == 'install' or ev.data.kind == 'update') then
      vim.cmd('TSUpdate')
    end
  end,
})
