local wezterm = require 'wezterm'
local act = wezterm.action

return {
--  color_scheme = 'Catppuccin Mocha',
  color_scheme = "Solarized (light) (terminal.sexy)",
  keys = {
    { key = 'LeftArrow', mods = 'CTRL', action = act.ActivateTabRelative(-1) },
    { key = 'RightArrow', mods = 'CTRL', action = act.ActivateTabRelative(1) },
    {
      key = '|',
      mods = 'CTRL|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Right',
        command = { args = { 'zsh' } },
	      size = { Percent = 50 },
      }
    },
    {
      key = '|',
      mods = 'CMD|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Right',
        command = { args = { 'zsh' } },
	      size = { Percent = 50 },
      }
    },
    {
      key = '_',
      mods = 'CTRL|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Down',
        command = { args = { 'zsh' } },
        size = { Percent = 50 },
      }
    },
    {
      key = '_',
      mods = 'CMD|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Down',
        command = { args = { 'zsh' } },
        size = { Percent = 50 },
      }
    },
    { key = ']', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Right' },
    { key = ']', mods = 'CMD',  action = wezterm.action.ActivatePaneDirection 'Right' },
    { key = '[', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Left'  },
    { key = '[', mods = 'CMD',  action = wezterm.action.ActivatePaneDirection 'Left'  },
    { key = 'UpArrow', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Up' },
    { key = 'UpArrow', mods = 'CMD', action = wezterm.action.ActivatePaneDirection 'Up' },
    { key = 'DownArrow', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Down' },
    { key = 'DownArrow', mods = 'CMD', action = wezterm.action.ActivatePaneDirection 'Down' },
    { key = 'PageUp', mods = 'CTRL', action = act.ScrollByPage(-0.5) },
    { key = 'PageDown', mods = 'CTRL', action = act.ScrollByPage(0.5) },
    { key = 'W', mods = 'CTRL', action = act.CloseCurrentTab {confirm = true}}
  }
}

