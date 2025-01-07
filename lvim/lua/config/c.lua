vim.api.nvim_create_autocmd('FileType', {
  pattern = { "c", "cpp" },
  callback = function()
    vim.opt.colorcolumn = "81"
    vim.opt.shiftwidth = 8
    vim.opt.expandtab = false
  end
})
