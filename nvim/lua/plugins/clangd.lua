local nix_clangd = "/run/current-system/sw/bin/clangd"
local has_nix_clangd = vim.fn.executable(nix_clangd) == 1

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        clangd = {
          mason = not has_nix_clangd,
          cmd = { has_nix_clangd and nix_clangd or "clangd" },
        },
      },
    },
  },
}
