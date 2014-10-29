
class dotfiles ($home = '/tmp') {

  $files = {
    'msmtprc'        => {
        mode         => 0600,
        path         => "${home}/.msmtprc",
        content      => template('dotfiles/msmtprc')},
    'Xresources'     => {
        mode         => 0644,
        path         => "${home}/.Xresources",
        content      => template('dotfiles/Xresources')},
    'vimrc'          => {
        mode         => 0644,
        path         => "${home}/.vimrc",
        content      => template('dotfiles/vimrc')},
    'zshrc'          => {
        mode         => 0644,
        path         => "${home}/.zshrc",
        content      => template('dotfiles/zshrc')},
    'my.cnf'         => {
        mode         => 0600,
        path         => "${home}/.my.cnf",
        content      => template('dotfiles/my.cnf')},
    'psqlrc'         => {
        mode         => 0644,
        path         => "${home}/.psqlrc",
        content      => template('dotfiles/psqlrc')},
    'gitconfig'      => {
        mode         => 0644,
        path         => "${home}/.gitconfig",
        content      => template('dotfiles/gitconfig')},
    'byobu'          => {
        mode         => 0644,
        path         => "${home}/.byobu",
        sourceselect => all,
        recurse      => true,
        source       => 'puppet:///modules/dotfiles/byobu'},
  }

  create_resources(file, $files)

}


