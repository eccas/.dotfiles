# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

#Touchpad settings. Comment if keyboard lacks touchpad.
synclient PalmDetect=1
synclient FingerHigh=30
synclient MinSpeed=0.35
synclient MaxSpeed=1.8
synclient AccelFactor=0.08
synclient AreaTopEdge=2500
synclient AreaBottomEdge=0
synclient RightButtonAreaTop=0
synclient RightButtonAreaLeft=3292
synclient MiddleButtonAreaRight=3292
synclient MiddleButtonAreaLeft=2600
