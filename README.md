saftladen
=========

An fancy cash desk system.


usage
=====

This project has some submodules. Therfore after a clone of the repository, the submodules have also to be 
initialized.

```
git submodule init
git submodule update
```
more information about submodules can be found on http://git-scm.com/book/en/Git-Tools-Submodules.


To live update the webserver during development, you can do some thing like the following.
```
sshfs gaf-sl:de.lmu.fs.gaf/htdocs/saftladen/$USER mnt
lsyncd -direct gitdir/saftladen/slrest/ mnt
```
