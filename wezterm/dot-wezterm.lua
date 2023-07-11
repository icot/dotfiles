local wezterm = require 'wezterm'
local act = wezterm.action

wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    if tab.is_active then
      return {
        { Background = { Color = 'black' } },
        { Text = ' ' .. tab.active_pane.title .. ' ' },
      }
    end
    return tab.active_pane.title
  end
)

return {

  window_decorations = "RESIZE",
  -- color_scheme = 'Catppuccin Macchiato',
  color_scheme = 'Chalk',
  -- window_background_opacity = 0.99,
  font_size = 13.0,

  
  window_frame = {
    active_titlebar_bg = '#666666',
    inactive_titlebar_bg = '#666666',
  },

  keys = {
    { key = 'LeftArrow', mods = 'CTRL', action = act.ActivateTabRelative(-1) },
    { key = 'RightArrow', mods = 'CTRL', action = act.ActivateTabRelative(1) },
    { key = 'LeftArrow', mods = 'CMD', action = act.ActivateTabRelative(-1) },
    { key = 'RightArrow', mods = 'CMD', action = act.ActivateTabRelative(1) },
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
      key = 'd',
      mods = 'CMD',
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
      key = 'd',
      mods = 'CMD|SHIFT',
      action = wezterm.action.SplitPane {
        direction = 'Down',
        command = { args = { 'zsh' } },
        size = { Percent = 50 },
      }
    },
    { key = ']', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Next' },
    { key = ']', mods = 'CMD',  action = wezterm.action.ActivatePaneDirection 'Prev' },
    { key = '[', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Next'  },
    { key = '[', mods = 'CMD',  action = wezterm.action.ActivatePaneDirection 'Left'  },
    { key = 'UpArrow', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Prev' },
    { key = 'UpArrow', mods = 'CMD', action = wezterm.action.ActivatePaneDirection 'Prev' },
    { key = 'DownArrow', mods = 'CTRL', action = wezterm.action.ActivatePaneDirection 'Next' },
    { key = 'DownArrow', mods = 'CMD', action = wezterm.action.ActivatePaneDirection 'Next' },
    { key = 'PageUp', mods = 'CTRL', action = act.ScrollByPage(-0.5) },
    { key = 'PageDown', mods = 'CTRL', action = act.ScrollByPage(0.5) },
    { key = 'Space', mods = 'CTRL', action = act.PaneSelect },
    { key = '0', mods = 'CTRL', action = act.PaneSelect { mode = 'SwapWithActive'} },
    { key = 'Space', mods = 'CMD', action = act.PaneSelect },
    { key = '0', mods = 'CMD', action = act.PaneSelect { mode = 'SwapWithActive'} },
--    { key = 't', mods = 'CTRL', action = wezterm.action.SpawnTab 'CurrentPaneDomain' }
--    { key = 'w', mods = 'CTRL', action = wezterm.action.CloseCurrentTab{confirm=true}}
  }
}
