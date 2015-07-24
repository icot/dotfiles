dotfiles
========

My  personal collection of dotfiles

Fill dotfiles.yaml and put it on the appropiate folder.
In my case (Debian Testing):

First copy *hiera.yaml* and *dotfiles.yaml* to ~/.puppet/
and ~/.pupet/hieradata/ respectively:

    /home/<username>/.puppet/hiera.yaml
    /home/<username>/.puppet/hieradata/dotfiles.yaml

Then run:

    $ puppet apply --modulepath=<PARENT_FOLDER> deploy.pp


