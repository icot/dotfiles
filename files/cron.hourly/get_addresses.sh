#!/bin/bash

find ~/mail/CERN/INBOX/ -type f -mtime -7 -print0 | \
      xargs -0 -n 1 -r /bin/bash -c 'lbdb-fetchaddr -a < "$1"' lbdb-fetchaddr

# remove dups
SORT_OUTPUT=name /usr/lib/lbdb/lbdb-munge
