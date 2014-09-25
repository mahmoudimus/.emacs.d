# .emacs.d - the eternal config

This emacs customization relies on the wonderful [Prelude](https://github.com/bbatsov/prelude) library that just makes Emacs a lovely editor

## Getting Started

```bash
git clone --recursive https://github.com/mahmoudimus/.emacs.d.git ~/.emacs.d
cd ~/.emacs.d
./sync init
```

### Personalizations

All of my personalizations are in the `personal`, `themes`, and `vendor` folder. You can follow suit as well.

## Update prelude

Got this trick from: http://scribu.net/blog/git-alias-for-updating-submodules.html

First, run this:

```bash
git config --global alias.up-sub '!f() { cd $1 && git checkout master && git pull && git submodule update --init --recursive; }; f
```

Second: `./sync update-prelude` or `git up-sub prelude` from the `~/.emacs.d/` directory.
