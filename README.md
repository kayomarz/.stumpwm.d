My StumpWM init.

# StumpWM with SLIME

To intertact with StumpWM using SLIME refer to the following article:

[www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/](https://www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/)

Accordingly in [init.lisp](./init.lisp) run a swank server on port 4004 instead
of the usual 4005. To quote the article:

> Going back to the .stumpwmrc, notice how the port is set to 4004? I do that so
> that when you start SLIME in Emacs, there are no errors because the default
> port is actually 4005. This ensures you can't mess up your WM by accident
> while writing unrelated code.

## ~/.xinitrc

Although not part of StumpWM init, in order to interact with SLIME, make a
change in `~/.xinitrc` so that startx runs sbcl and loads stumpwm as the last
line of the .xinitrc as follows:

    exec sbcl --load ~/.stumpwm.d/startstump

(If we dont want to interact with StumpWM via SLIME, the last line can be
something like /usr/loca/bin/stumpwm.)

# Customization

See [init.lisp](./init.lisp).