# Usage

My dotfiles are managed the [openSUSE way](https://news.opensuse.org/2020/03/27/Manage-dotfiles-with-Git/).

```
$ git clone --bare https://github.com/ludwigd/dotfiles $HOME/.dotfiles
$ git --git-dir=$HOME/.dotfiles --work-tree=$HOME checkout -f
```
Now you have access to the `dotfiles` command, which is an alias for `git --git-dir=$HOME/.dotfiles --work-tree=$HOME`.
