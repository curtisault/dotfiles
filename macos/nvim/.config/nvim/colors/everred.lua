-- everred — a warm maroon colorscheme modeled on Everforest's eye-comfort design.
-- Rules borrowed from Everforest: low-contrast warm-dark bg ramp, cream fg,
-- grayed accents. Anchored to 5 color families: maroon/red, cream, gold,
-- orange, green (no blue/aqua/purple). Background is a deep oxblood maroon.
-- Load with:  vim.cmd[[colorscheme everred]]

vim.cmd('highlight clear')
if vim.fn.exists('syntax_on') == 1 then
  vim.cmd('syntax reset')
end
vim.o.background = 'dark'
vim.g.colors_name = 'everred'

-- Palette anchored to 5 color families: maroon/red, cream, gold, orange, green.
-- The variable names below (blue/aqua/purple) are retained only so the highlight
-- groups don't change — their VALUES are shades of orange/gold, not blue tones.
local c = {
  -- ── MAROON / DARK RED ── backgrounds (deep oxblood ramp) + hero red
  bg_dim = '#1c0f13',
  bg0    = '#25141a', -- main editor background (deep maroon)
  bg1    = '#301a21', -- cursorline, statusline, floats
  bg2    = '#3c222a',
  bg3    = '#492a34', -- selection / border
  bg4    = '#56333e',
  bg5    = '#643d49',
  red    = '#e9645d', -- bright red: keywords, errors, deletes
  bg_red    = '#4d222a', -- error / delete tint
  bg_visual = '#4f2b3a', -- selection tint

  -- ── GREEN ── strings, diff-add
  green    = '#a8c47e',
  bg_green = '#36362a', -- diff-add tint

  -- ── CREAM ── foreground, variables, comments (dusty rose-grey)
  fg    = '#d8c6aa',
  grey0 = '#8c7174', -- comments (maroon-tinted, in-family)
  grey1 = '#a8908b', -- line numbers / punctuation
  grey2 = '#c5aba4', -- brighter neutral

  -- ── GOLD ── functions, titles, special/escapes
  yellow    = '#e6bd71', -- gold: functions, titles
  aqua      = '#dcc583', -- soft gold: special, escapes, preproc
  bg_yellow = '#46361f', -- change / warn tint

  -- ── ORANGE ── constants, numbers, types, operators
  orange = '#ec9a5c', -- operators, labels, constants
  blue   = '#e3895a', -- terracotta: types, properties, fields
  purple = '#df8b6f', -- coral: numbers, booleans, builtins
  bg_blue = '#322628', -- info tint (warm)

  none = 'NONE',
}

local function hi(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

local groups = {
  -- ── editor UI ────────────────────────────────────────────────────────
  Normal       = { fg = c.fg, bg = c.bg0 },
  NormalNC     = { fg = c.fg, bg = c.bg0 },
  NormalFloat  = { fg = c.fg, bg = c.bg1 },
  FloatBorder  = { fg = c.grey2, bg = c.bg1 },
  FloatTitle   = { fg = c.orange, bg = c.bg1, bold = true },
  ColorColumn  = { bg = c.bg1 },
  Cursor       = { fg = c.bg0, bg = c.fg },
  CursorLine   = { bg = c.bg1 },
  CursorColumn = { bg = c.bg1 },
  CursorLineNr = { fg = c.orange, bold = true },
  LineNr       = { fg = c.bg5 },
  SignColumn   = { fg = c.fg, bg = c.bg0 },
  FoldColumn   = { fg = c.grey0, bg = c.bg0 },
  Folded       = { fg = c.grey1, bg = c.bg1 },
  WinSeparator = { fg = c.bg4, bg = c.bg0 },
  VertSplit    = { fg = c.bg4, bg = c.bg0 },
  Visual       = { bg = c.bg_visual },
  VisualNOS    = { bg = c.bg_visual },
  Search       = { fg = c.bg0, bg = c.yellow },
  IncSearch    = { fg = c.bg0, bg = c.orange },
  CurSearch    = { fg = c.bg0, bg = c.orange },
  MatchParen   = { fg = c.orange, bold = true, underline = true },
  NonText      = { fg = c.bg4 },
  Whitespace   = { fg = c.bg4 },
  SpecialKey   = { fg = c.bg4 },
  EndOfBuffer  = { fg = c.bg0 },
  Conceal      = { fg = c.grey0 },
  Directory    = { fg = c.green },
  Title        = { fg = c.orange, bold = true },
  QuickFixLine = { bg = c.bg2, bold = true },

  -- spell
  SpellBad   = { undercurl = true, sp = c.red },
  SpellCap   = { undercurl = true, sp = c.yellow },
  SpellRare  = { undercurl = true, sp = c.purple },
  SpellLocal = { undercurl = true, sp = c.aqua },

  -- popup menu / wildmenu
  Pmenu        = { fg = c.fg, bg = c.bg1 },
  PmenuSel     = { fg = c.bg0, bg = c.green, bold = true },
  PmenuKind    = { fg = c.purple, bg = c.bg1 },
  PmenuExtra   = { fg = c.grey1, bg = c.bg1 },
  PmenuSbar    = { bg = c.bg3 },
  PmenuThumb   = { bg = c.grey0 },
  PmenuMatch    = { fg = c.orange, bg = c.bg1, bold = true },
  PmenuMatchSel = { fg = c.bg0, bg = c.green, bold = true },
  WildMenu     = { fg = c.bg0, bg = c.green },

  -- messages
  ErrorMsg   = { fg = c.red, bold = true },
  WarningMsg = { fg = c.yellow, bold = true },
  ModeMsg    = { fg = c.fg },
  MoreMsg    = { fg = c.green },
  Question   = { fg = c.yellow },
  MsgArea    = { fg = c.fg },

  -- statusline / tabline
  StatusLine   = { fg = c.fg, bg = c.bg2 },
  StatusLineNC = { fg = c.grey0, bg = c.bg1 },
  TabLine      = { fg = c.grey1, bg = c.bg1 },
  TabLineFill  = { bg = c.bg_dim },
  TabLineSel   = { fg = c.bg0, bg = c.green, bold = true },

  -- ── base syntax ──────────────────────────────────────────────────────
  Comment        = { fg = c.grey0, italic = true },
  SpecialComment = { fg = c.grey1, italic = true },
  Todo           = { fg = c.bg0, bg = c.yellow, bold = true },
  Error          = { fg = c.red, bold = true },

  Constant  = { fg = c.purple },
  String    = { fg = c.yellow },
  Character = { fg = c.yellow },
  Number    = { fg = c.purple },
  Boolean   = { fg = c.purple },
  Float     = { fg = c.purple },

  Identifier = { fg = c.fg },
  Function   = { fg = c.fg },

  Statement   = { fg = c.green, italic = true },
  Conditional = { fg = c.green, italic = true },
  Repeat      = { fg = c.green, italic = true },
  Label       = { fg = c.grey1 },
  Operator    = { fg = c.grey1 },
  Keyword     = { fg = c.green, italic = true },
  Exception   = { fg = c.green, italic = true },

  PreProc   = { fg = c.aqua },
  Include   = { fg = c.aqua },
  Define    = { fg = c.aqua },
  Macro     = { fg = c.aqua },
  PreCondit = { fg = c.aqua },

  Type         = { fg = c.blue, bold = true },
  StorageClass = { fg = c.green, italic = true },
  Structure    = { fg = c.blue, bold = true },
  Typedef      = { fg = c.blue, bold = true },

  Special     = { fg = c.aqua },
  SpecialChar = { fg = c.aqua },
  Tag         = { fg = c.orange },
  Delimiter   = { fg = c.grey1 },
  Debug       = { fg = c.red },
  Underlined  = { fg = c.blue, underline = true },
  Ignore      = { fg = c.grey0 },

  -- ── diagnostics ──────────────────────────────────────────────────────
  DiagnosticError = { fg = c.red },
  DiagnosticWarn  = { fg = c.yellow },
  DiagnosticInfo  = { fg = c.blue },
  DiagnosticHint  = { fg = c.aqua },
  DiagnosticOk    = { fg = c.green },
  DiagnosticUnderlineError = { undercurl = true, sp = c.red },
  DiagnosticUnderlineWarn  = { undercurl = true, sp = c.yellow },
  DiagnosticUnderlineInfo  = { undercurl = true, sp = c.blue },
  DiagnosticUnderlineHint  = { undercurl = true, sp = c.aqua },
  DiagnosticVirtualTextError = { fg = c.red, bg = c.bg_red },
  DiagnosticVirtualTextWarn  = { fg = c.yellow, bg = c.bg_yellow },
  DiagnosticVirtualTextInfo  = { fg = c.blue, bg = c.bg_blue },
  DiagnosticVirtualTextHint  = { fg = c.aqua, bg = c.bg_green },

  -- ── LSP ──────────────────────────────────────────────────────────────
  LspReferenceText  = { bg = c.bg2 },
  LspReferenceRead  = { bg = c.bg2 },
  LspReferenceWrite = { bg = c.bg2, underline = true },
  LspInlayHint      = { fg = c.grey0, bg = c.bg1, italic = true },
  LspSignatureActiveParameter = { fg = c.orange, bold = true },

  -- ── treesitter ───────────────────────────────────────────────────────
  ['@variable']          = { fg = c.fg },
  ['@variable.builtin']  = { fg = c.purple }, -- self / this / __MODULE__
  ['@variable.parameter']= { fg = c.fg },
  ['@variable.parameter.builtin'] = { fg = c.purple },
  ['@variable.member']   = { fg = c.fg },
  ['@constant']          = { fg = c.purple },
  ['@constant.builtin']  = { fg = c.purple },
  ['@constant.macro']    = { fg = c.red },
  ['@module']            = { fg = c.blue, bold = true },
  ['@module.builtin']    = { fg = c.blue, bold = true },
  ['@string']            = { fg = c.yellow },
  ['@string.documentation'] = { fg = c.grey0, italic = true }, -- docstrings read like comments
  ['@string.regexp']     = { fg = c.orange },  -- regex distinct from plain strings
  ['@string.escape']     = { fg = c.aqua },
  ['@string.special']    = { fg = c.aqua },
  ['@string.special.symbol'] = { fg = c.green }, -- atoms / ecto keyword keys
  ['@string.special.url'] = { fg = c.blue, underline = true },
  ['@label']             = { fg = c.green },      -- keyword-list keys (where:, preload:)
  ['@character']         = { fg = c.yellow },
  ['@character.special'] = { fg = c.aqua },
  ['@number']            = { fg = c.purple },
  ['@number.float']      = { fg = c.purple },
  ['@boolean']           = { fg = c.purple },
  ['@function']          = { fg = c.fg, bold = true },
  ['@function.builtin']  = { fg = c.fg },
  ['@function.call']     = { fg = c.fg },
  ['@function.method']   = { fg = c.fg },
  ['@function.macro']    = { fg = c.red },
  ['@constructor']       = { fg = c.blue, bold = true },
  ['@keyword']             = { fg = c.green, italic = true },
  ['@keyword.function']    = { fg = c.green, italic = true },
  ['@keyword.return']      = { fg = c.green, italic = true },
  ['@keyword.operator']    = { fg = c.green, italic = true }, -- and / or / not / in
  ['@keyword.conditional'] = { fg = c.green, italic = true },
  ['@keyword.repeat']      = { fg = c.green, italic = true },
  ['@keyword.exception']   = { fg = c.green, italic = true }, -- try / rescue / raise
  ['@keyword.coroutine']   = { fg = c.green, italic = true }, -- async / await / yield
  ['@keyword.modifier']    = { fg = c.green, italic = true }, -- pub / static / const / mut
  ['@keyword.type']        = { fg = c.green, italic = true }, -- struct / enum / class
  ['@keyword.import']      = { fg = c.aqua },  -- import / use / require / #include
  ['@keyword.directive']   = { fg = c.aqua },  -- #define / @derive / pragmas
  ['@keyword.directive.define'] = { fg = c.aqua },
  ['@conditional']         = { fg = c.green, italic = true }, -- legacy capture
  ['@repeat']              = { fg = c.green, italic = true }, -- legacy capture
  ['@operator']            = { fg = c.grey1 },
  ['@type']                = { fg = c.blue, bold = true },
  ['@type.builtin']        = { fg = c.blue, bold = true },
  ['@type.definition']     = { fg = c.blue, bold = true },
  ['@attribute']           = { fg = c.red },
  ['@attribute.builtin']   = { fg = c.red },
  ['@property']            = { fg = c.fg },
  ['@field']               = { fg = c.fg },
  ['@punctuation.delimiter'] = { fg = c.grey1 },
  ['@punctuation.bracket']   = { fg = c.grey1 },
  ['@punctuation.special']   = { fg = c.orange },
  ['@comment']           = { fg = c.grey0, italic = true },
  ['@tag']               = { fg = c.orange },
  ['@tag.builtin']       = { fg = c.orange },
  ['@tag.attribute']     = { fg = c.green },
  ['@tag.delimiter']     = { fg = c.grey2 },
  ['@markup.heading']    = { fg = c.orange, bold = true },
  ['@markup.link']       = { fg = c.blue, underline = true },
  ['@markup.link.label'] = { fg = c.blue },
  ['@markup.link.url']   = { fg = c.grey1, underline = true },
  ['@markup.raw']        = { fg = c.yellow },
  ['@markup.raw.markdown_inline'] = { fg = c.yellow },
  ['@markup.strong']        = { bold = true },
  ['@markup.italic']        = { italic = true },
  ['@markup.strikethrough'] = { strikethrough = true },
  ['@markup.quote']         = { fg = c.grey1, italic = true },
  ['@markup.list']          = { fg = c.orange },
  -- colored tags inside comments (TODO / NOTE / FIXME / etc.)
  ['@comment.todo']      = { fg = c.bg0, bg = c.yellow, bold = true },
  ['@comment.note']      = { fg = c.bg0, bg = c.aqua, bold = true },
  ['@comment.warning']   = { fg = c.bg0, bg = c.orange, bold = true },
  ['@comment.error']     = { fg = c.bg0, bg = c.red, bold = true },
  ['@diff.plus']         = { fg = c.green },
  ['@diff.minus']        = { fg = c.red },
  ['@diff.delta']        = { fg = c.yellow },

  -- ── git / diff ───────────────────────────────────────────────────────
  DiffAdd    = { bg = c.bg_green },
  DiffChange = { bg = c.bg_yellow },
  DiffDelete = { bg = c.bg_red },
  DiffText   = { bg = '#5a4633' },
  diffAdded   = { fg = c.green },
  diffRemoved = { fg = c.red },
  diffChanged = { fg = c.yellow },
  diffFile    = { fg = c.orange },
  diffLine    = { fg = c.grey1 },
  GitSignsAdd    = { fg = c.green },
  GitSignsChange = { fg = c.yellow },
  GitSignsDelete = { fg = c.red },

  -- ── plugins: nvim-tree, oil, render-markdown ─────────────────────────
  NvimTreeNormal       = { fg = c.fg, bg = c.bg_dim },
  NvimTreeFolderIcon   = { fg = c.orange },
  NvimTreeFolderName   = { fg = c.green },
  NvimTreeOpenedFolderName = { fg = c.green, bold = true },
  NvimTreeRootFolder   = { fg = c.purple, bold = true },
  NvimTreeGitDirty     = { fg = c.yellow },
  NvimTreeGitNew       = { fg = c.green },
  NvimTreeGitDeleted   = { fg = c.red },
  NvimTreeSpecialFile  = { fg = c.orange, underline = true },
  NvimTreeIndentMarker = { fg = c.bg4 },

  OilDir     = { fg = c.green },
  OilDirIcon = { fg = c.orange },
  OilLink    = { fg = c.blue, underline = true },
  OilCreate  = { fg = c.green },
  OilDelete  = { fg = c.red },
  OilMove    = { fg = c.yellow },
  OilCopy    = { fg = c.aqua },

  RenderMarkdownH1Bg = { fg = c.red, bg = c.bg_red, bold = true },
  RenderMarkdownH2Bg = { fg = c.orange, bg = c.bg_yellow, bold = true },
  RenderMarkdownH3Bg = { fg = c.yellow, bg = c.bg2 },
  RenderMarkdownH4Bg = { fg = c.green, bg = c.bg2 },
  RenderMarkdownH5Bg = { fg = c.aqua, bg = c.bg1 },
  RenderMarkdownH6Bg = { fg = c.blue, bg = c.bg1 },
  RenderMarkdownCode = { bg = c.bg_dim },
  RenderMarkdownBullet = { fg = c.orange },
  RenderMarkdownLink   = { fg = c.blue, underline = true },

  -- mini.nvim (icons / statusline fallbacks)
  MiniStatuslineModeNormal  = { fg = c.bg0, bg = c.green, bold = true },
  MiniStatuslineModeInsert  = { fg = c.bg0, bg = c.blue, bold = true },
  MiniStatuslineModeVisual  = { fg = c.bg0, bg = c.purple, bold = true },
  MiniStatuslineModeReplace = { fg = c.bg0, bg = c.red, bold = true },
  MiniStatuslineModeCommand = { fg = c.bg0, bg = c.orange, bold = true },
}

for group, opts in pairs(groups) do
  hi(group, opts)
end

-- terminal colors
vim.g.terminal_color_0  = c.bg1
vim.g.terminal_color_1  = c.red
vim.g.terminal_color_2  = c.green
vim.g.terminal_color_3  = c.yellow
vim.g.terminal_color_4  = c.blue
vim.g.terminal_color_5  = c.purple
vim.g.terminal_color_6  = c.aqua
vim.g.terminal_color_7  = c.fg
vim.g.terminal_color_8  = c.grey0
vim.g.terminal_color_9  = c.red
vim.g.terminal_color_10 = c.green
vim.g.terminal_color_11 = c.yellow
vim.g.terminal_color_12 = c.blue
vim.g.terminal_color_13 = c.purple
vim.g.terminal_color_14 = c.aqua
vim.g.terminal_color_15 = c.grey2
