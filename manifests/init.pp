
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
    'xmonad'         => {
        mode         => 0644,
        path         => "${home}/.xmonad",
        sourceselect => all,
        recurse      => true,
        source       => 'puppet:///modules/dotfiles/xmonad'},
    'mutt'           => {
        mode         => 0644,
        path         => "${home}/.mutt",
        sourceselect => all,
        recurse      => true,
        source       => 'puppet:///modules/dotfiles/mutt'},
    'mutt-acc-cern'  => {
        require      => File['mutt'],
        mode         => 0600,
        path         => "${home}/.mutt/account.cern",
        content      => template('dotfiles/mutt/account.cern')},
    'profanity-shr'  => {
        ensure       => directory,
        path         => "${home}/.local/share/profanity",},
    'profanity-acc'  => {
        require      => File['profanity-shr'],
        mode         => 0600,
        path         => "${home}/.local/share/profanity/accounts",
        content      => template('dotfiles/local/share/profanity/accounts')},
    'profanity-cnf'  => {
        ensure       => directory,
        path         => "${home}/.config/profanity",},
    'profanity-rc '  => {
        require      => File['profanity-cnf'],
        mode         => 0644,
        path         => "${home}/.config/profanity/profrc",
        source       => 'puppet:///modules/dotfiles/config/profanity/profrc'},
  }

  create_resources(file, $files)

}


