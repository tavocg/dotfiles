local vars = require("./hyprland.d/01-vars.lua")
local mod = vars.mod

hl.bind(mod .. " + RETURN", hl.dsp.exec_cmd(vars.term))
hl.bind(mod .. " + R", hl.dsp.exec_cmd(vars.menu))
hl.bind(mod .. " + P", hl.dsp.exec_cmd(vars.power))
hl.bind(mod .. " + SHIFT + P", hl.dsp.exec_cmd(vars.pass))
hl.bind(mod .. " + SHIFT + O", hl.dsp.exec_cmd(vars.passotp))
hl.bind(mod .. " + O", hl.dsp.exec_cmd(vars.sound))
hl.bind(mod .. " + I", hl.dsp.exec_cmd(vars.wifi))
hl.bind(mod .. " + U", hl.dsp.exec_cmd(vars.bluetooth))
hl.bind(mod .. " + E", hl.dsp.exec_cmd(vars.filemanager))
hl.bind(mod .. " + W", hl.dsp.exec_cmd(vars.browser))

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(vars.volup), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(vars.voldown), { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd(vars.mute), { locked = true, repeating = true })
hl.bind("SHIFT + XF86AudioRaiseVolume", hl.dsp.exec_cmd(vars.micup), { locked = true, repeating = true })
hl.bind("SHIFT + XF86AudioLowerVolume", hl.dsp.exec_cmd(vars.micdown), { locked = true, repeating = true })
hl.bind("SHIFT + XF86AudioMute", hl.dsp.exec_cmd(vars.micmute), { locked = true, repeating = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd(vars.micmute), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd(vars.brightup), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd(vars.brightdown), { locked = true, repeating = true })
hl.bind("Print", hl.dsp.exec_cmd(vars.prt), { locked = true, repeating = true })
hl.bind(mod .. " + SHIFT + S", hl.dsp.exec_cmd(vars.prt))
hl.bind(mod .. " + SHIFT + C", hl.dsp.exec_cmd(vars.colorpicker))

for workspace = 1, 10 do
    local key = workspace % 10

    hl.bind(mod .. " + " .. key, hl.dsp.focus({ workspace = workspace }))
    hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = workspace }))
end

hl.bind(mod .. " + SHIFT + Q", hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(mod .. " + C", hl.dsp.window.close())
hl.bind(mod .. " + SHIFT + F", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mod .. " + F", hl.dsp.window.fullscreen(0))
hl.bind(mod .. " + SHIFT + t", hl.dsp.group.toggle())

hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
