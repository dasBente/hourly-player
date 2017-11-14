# hourly-player
Script that can be schduled with crontab to play hourly sound files using aplay.

## Installation

Use `git clone https://github.com/dasbente/hourly-player` to download the reposiroy to a
directory of your choosing.

### Using hourly-player with crontab

Not a lot of work is involved to get crontab to automatically schedule your hourlies to play
at the beginning of every hour. For this you can either use a file to hold your schedules and
feed it to crontab using `crontab /path/to/my_crontab` (you can use example_crontab in the example
folder of this repository as a template) or directly edit your crontab schedule using `crontab -e`.

For the hourly player to run using crontab you also need to determine your XDG runtime directory
(you can use `echo $XDG_RUNTIME_DIR` to find this). This is needed for aplay to work.

You can also use the `@reboot` rule to launch the tray icon when booting. This currently requires SBCL
to work (also, the scripts dependencies may or may not need to be manually installed using quicklisp).

A working crontab should look something like this

```bash
XDG_RUNTIME_DIR=/run/user/1000 # or whatever is returned by echo $XDG_RUNTIME_DIR

# Schedules
# ...

0 * * * * /path/to/play_hourly # You can also use ">& some.log" to redirect any output to a log file
@reboot /path/to/hourly-player/run # requires SBCL
# ...
```

### Launching the tray icon from crontab

For the above autostart of the tray icon to work, some adjustments to the current version of
`tray-icon.lisp` need to be made. You need to adjust the paths defined at the start of the script:
 * `*path-to-play-hourly*` needs to point to the place you put play_hourly (e.g. the location of
   this repo on your system)
 * `*path-to-icon*` needs to point to some image you want to use as a icon
 