local vars = require("./hyprland.d/01-vars.lua")
local mod = vars.mod

hl.config({
	general = {
		layout = "master",
	},

	master = {
		new_status = "slave",
		new_on_top = false,
	},
})

hl.bind(mod .. " + H", hl.dsp.focus({ direction = "left" }))
hl.bind(mod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mod .. " + K", hl.dsp.layout("cycleprev"))
hl.bind(mod .. " + J", hl.dsp.layout("cyclenext"))
hl.bind(mod .. " + left", hl.dsp.focus({ direction = "left" }))
hl.bind(mod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mod .. " + up", hl.dsp.focus({ direction = "up" }))
hl.bind(mod .. " + down", hl.dsp.focus({ direction = "down" }))

hl.bind(mod .. " + SHIFT + H", hl.dsp.layout("mfact -0.05"))
hl.bind(mod .. " + SHIFT + L", hl.dsp.layout("mfact +0.05"))
hl.bind(mod .. " + SHIFT + K", hl.dsp.layout("swapprev"))
hl.bind(mod .. " + SHIFT + J", hl.dsp.layout("swapnext"))

hl.bind(mod .. " + SHIFT + I", hl.dsp.layout("addmaster"))
hl.bind(mod .. " + SHIFT + D", hl.dsp.layout("removemaster"))
