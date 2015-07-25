
class dotfiles::packages {

  # Install packages
  $packages = hiera('packages')
  create_resources(package, $packages)

}

include dotfiles::packages
