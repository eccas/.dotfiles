# Generic .zshenv file for zsh
#
# .zshenv is sourced on ALL invocations of the shell, unless the -f option is
# set.  It should NOT normally contain commands to set the command search path,
# or other common environment variables unless you really know what you're
# doing.  E.g. running "PATH=/custom/path gdb program" sources this file (when
# gdb runs the program via $SHELL), so you want to be sure not to override a
# custom environment in such cases.  Note also that .zshenv should not contain
# commands that produce output or assume the shell is attached to a tty.
#
# Some people insist on setting their PATH here to affect things like ssh.
# Those that do should probably use $SHLVL to ensure that this only happens
# the first time the shell is started (to avoid overriding a customized
# environment).  Also, the various profile/rc/login files all get sourced
# *after* this file, so they will override this value.  One solution is to
# put your path-setting code into a file named .zpath, and source it from
# both here (if we're not a login shell) and from the .zprofile file (which
# is only sourced if we are a login shell).
if [[ $SHLVL == 1 && ! -o LOGIN ]]; then
    source ~/.zpath
fi
