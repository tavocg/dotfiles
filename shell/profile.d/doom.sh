#!/bin/sh
# + `doom sync` to synchronize your private config with Doom by installing missing
#   packages, removing orphaned packages, and regenerating caches. Run this
#   whenever you modify your private `init.el` or `packages.el`, or install/remove
#   an Emacs package through your OS package manager (e.g. mu4e or agda).
# + `doom upgrade` to update Doom to the latest release & all installed packages.
# + `doom doctor` to diagnose common issues with your system and config.
# + `doom env` to dump a snapshot of your shell environment to a file that Doom
#   will load at startup. This allows Emacs to inherit your `PATH`, among other
#   things.

export PATH="$HOME/.config/emacs/bin${PATH:+:${PATH}}"
