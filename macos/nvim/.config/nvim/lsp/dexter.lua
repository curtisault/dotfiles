-- dexter has no nvim-lspconfig entry, so mason-lspconfig won't auto-enable it.
return {
  cmd = { 'dexter', 'lsp' },
  filetypes = { 'elixir', 'eelixir', 'heex', 'surface' },
  root_markers = { 'mix.exs', '.git' },
}
