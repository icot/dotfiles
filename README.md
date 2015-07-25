dotfiles
========

My  personal collection of dotfiles

Fill dotfiles.yaml and put it on the appropiate folder.
In my case (Debian Testing):

First edit *hiera.yaml* and copy it to one of this locations to
configure the hiera installation:
- /etc/hiera/
- /etc/puppet/
- ~/.puppet/

and copy *dotfiles.yaml* to 
- /home/<username>/.puppet/hieradata/dotfiles.yaml

Then run:

    $ sudo puppet apply manifests/packages.pp # To install packages
    $ puppet apply --modulepath=<PARENT_FOLDER> deploy.pp # To set conf files


