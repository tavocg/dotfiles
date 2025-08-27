return {
  -- 1. Vim syntax plugin for Templ files
  {
    "joerdav/templ.vim",
    ft = "templ",
  },

  -- 2. Tree-sitter support
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "templ" })
    end,
  },

  -- 3. LSP configuration
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        templ = {}, -- templ LSP config with default options
      },
    },
  },
}
