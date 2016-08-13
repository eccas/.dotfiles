#!/bin/bash

# This will setup up a basic environment with emacs, zsh, terminal colors
# and some additional settings. The old environment will be backed up to
# $HOME/migrate.

SETUPDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Backup existing env
mkdir -p $HOME/migrate
MIGRATEDIR=$HOME/migrate
if [ -e $HOME/.emacs.d          ]; then mv $HOME/.emacs.d          $MIGRATEDIR ; fi
if [ -e $HOME/.emacs            ]; then mv $HOME/.emacs            $MIGRATEDIR ; fi
if [ -e $HOME/.bashrc           ]; then mv $HOME/.bashrc           $MIGRATEDIR ; fi
if [ -e $HOME/.spacemacs        ]; then mv $HOME/.spacemacs        $MIGRATEDIR ; fi
if [ -e $HOME/.gitconfig        ]; then mv $HOME/.gitconfig        $MIGRATEDIR ; fi
if [ -e $HOME/.gitignore_global ]; then mv $HOME/.gitignore_global $MIGRATEDIR ; fi
if [ -e $HOME/.dir_colors       ]; then mv $HOME/.dir_colors       $MIGRATEDIR ; fi
if [ -e $HOME/.zshrc            ]; then mv $HOME/.zshrc            $MIGRATEDIR ; fi

# Install emacs and spacemacs
sudo apt-get install emacs24 emacs24-common emacs24-common-non-dfsg emacs24-el emacs24-dbg
git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d

# Install zsh and oh-my-zsh
sudo apt-get install zsh zsh-doc
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
mkdir -p $HOME/.oh-my-zsh/custom/themes
cp $SETUPDIR/zsh/af-magic.zsh-theme $HOME/.oh-my-zsh/custom/themes/

# Install base16-gnome-terminal
git clone https://github.com/chriskempson/base16-gnome-terminal.git ~/.config/base16-gnome-terminal
source .config/base16-gnome-terminal/base16-monokai.dark.sh

# Install additional packages
sudo apt-get install xsel

# Setup symlinks
source $SETUPDIR/symlink-setup.sh
