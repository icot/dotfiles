dotfiles
========

My  personal collection of dotfiles

Fill dotfiles.yaml and put it on the appropiate foler.
In my case (Debian Jessy):

/home/<username>/.puppet/hieradata/dotfiles.yaml

In order to apply run:

 $ puppet apply --modulepath=<PARENT_FOLDER> deploy.pp


