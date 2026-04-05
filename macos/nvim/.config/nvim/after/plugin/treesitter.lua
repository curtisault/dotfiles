vim.api.nvim_create_autocmd('FileType', {
  pattern = '*',
  callback = function()
    local ft = vim.bo.filetype
    if ft == '' then return end
    local lang = vim.treesitter.language.get_lang(ft)
    if not lang then return end
    local ok = pcall(vim.treesitter.start)
    if not ok then
      require('nvim-treesitter.install').install({ lang })
    end
  end,
})

