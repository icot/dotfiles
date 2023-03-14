local wezterm = require 'wezterm'

return {
  color_scheme = 'Catppuccin Mocha',
  keys = {
    -- This will create a new split and run the `top` program inside it
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
      key = '_',
      mods = 'CTRL|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Down',
        command = { args = { 'zsh' } },
        size = { Percent = 50 },
      }
    },
    {
      key = ']',
      mods = 'CTRL',
      action = wezterm.action.ActivatePaneDirection 'Right'
    },
    {
      key = '[',
      mods = 'CTRL',
      action = wezterm.action.ActivatePaneDirection 'Left'
    },
    {
      key = 'UpArrow',
      mods = 'CTRL',
      action = wezterm.action.ActivatePaneDirection 'Up'
    },
    {
      key = 'DownArrow',
      mods = 'CTRL',
      action = wezterm.action.ActivatePaneDirection 'Down'
    }
  }
}

