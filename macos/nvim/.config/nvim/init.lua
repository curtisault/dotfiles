vim.opt.rtp:prepend("~/projects/agentflow.nvim")

require("curtis")
require("agentflow").setup({
  log = { level = "debug" },
})

