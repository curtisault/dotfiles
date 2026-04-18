vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities.completionProvider then
      vim.lsp.completion.enable(true, args.data.client_id, args.buf, { autotrigger = true })
    end
  end,
})

-- Confirm completion with <CR>
vim.keymap.set('i', '<CR>', function()
  if vim.fn.pumvisible() == 1 then
    return '<C-y>'
  end
  return '<CR>'
end, { expr = true })

-- Trigger manual completion
vim.keymap.set('i', '<C-o>', function()
  vim.lsp.completion.trigger()
end)
