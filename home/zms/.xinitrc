xsetroot -cursor_name left_ptr &
xset s off &
xset -dpms &
xrdb -merge ~/.Xdefaults &
#compton -b &
compton &
nitrogen --restore &
urxvtd --quiet --opendisplay --fork &
/usr/bin/chromium --no-startup-window &
synapse &
pulseaudio --start -D &
exec xmonad

