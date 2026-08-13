vim.opt.rtp:prepend("~/projects/agentflow.nvim")
vim.opt.rtp:prepend("~/projects/everdusk")

require("curtis")
require("agentflow").setup({
  log = { level = "debug" },
})

