
class dotfiles::packages {

  # Install packages
  $packages = hiera('packages')
  create_resources(package, $packages)
  
  $root_files = {
    'xmonad-session'   => {
        owner              => root,
        group              => root,
        mode               => 0644,
        path               => "/usr/bin/xmonad-session",
        source             => 'puppet:///modules/dotfiles/xmonad/xmonad-session'},
    'xmonad.desktop'   => {
        owner              => root,
        group              => root,
        mode               => 0644,
        path               => "/usr/share/xsessions/xmonad.desktop",
        source             => 'puppet:///modules/dotfiles/xmonad/xmonad-desktop'},
  }
  create_resources(file, $root_files)


}

include dotfiles::packages
