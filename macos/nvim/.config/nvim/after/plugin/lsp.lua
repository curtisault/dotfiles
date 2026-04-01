vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local bufnr = args.buf
    vim.keymap.set('n', '<C-]>', vim.lsp.buf.definition, { buffer = bufnr })
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = bufnr })
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = bufnr })
    vim.keymap.set('n', '<C-s>', vim.lsp.buf.signature_help, { buffer = bufnr })
    vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, { buffer = bufnr })
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { buffer = bufnr })
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { buffer = bufnr })
  end,
})

-- Open diagnostics on CursorHold
vim.api.nvim_create_autocmd('CursorHold', {
  callback = function()
    vim.diagnostic.open_float(nil, {
      focusable = false,
      close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
      border = 'rounded',
      source = 'always',
      prefix = ' ',
      scope = 'cursor',
    })
  end,
})

require('mason').setup({})
require('mason-lspconfig').setup({
  ensure_installed = { 'rust_analyzer' },
  handlers = {
    function(server_name)
      require('lspconfig')[server_name].setup({})
    end,
    ['lua_ls'] = function()
      require('lspconfig').lua_ls.setup({
        settings = {
          Lua = {
            workspace = { library = vim.api.nvim_get_runtime_file('', true) },
            telemetry = { enable = false },
          },
        },
      })
    end,
  },
})
