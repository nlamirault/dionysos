# dionysos

[![License GPL 2][badge-license]][LICENSE]
[![Coverage Status](https://coveralls.io/repos/nlamirault/dionysos/badge.png?branch=master)](https://coveralls.io/r/nlamirault/dionysos?branch=master)

Master :
* [![MELPA Stable](http://stable.melpa.org/packages/dionysos-badge.svg)](http://stable.melpa.org/#/dionysos)
* [![Circle CI](https://circleci.com/gh/nlamirault/dionysos/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/dionysos/tree/master)

Develop:
* [![Melpa Status](http://melpa.milkbox.net/packages/dionysos-badge.svg)](http://melpa.milkbox.net/#/dionysos)
* [![Circle CI](https://circleci.com/gh/nlamirault/dionysos/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/dionysos/tree/develop)




`dionysos` is a simple music player for Emacs.
Backends available are : [vlc][], [mplayer][]

## Installation

The recommended way to install ``dionysos`` is via [MELPA][]:

    M-x package-install dionysos

or [Cask][]:

	(depends-on "dionysos")

## Usage

### Backend

You could use this backends : **vlc**, **mplayer** and **mpd**.
Setup your backend :

    (setq dionysos-backend 'vlc)

### Directory listing

Listen to music files in a directory :

    M-x dionysos-files

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>RET</kbd>       | start playing file from the current line
<kbd>SPACE</kbd>     | stop music player
<kbd>n</kbd>         | play next song
<kbd>p</kbd>         | play previous song
<kbd>+</kbd>         | raise volume
<kbd>-</kbd>         | lower volume

### Playlist using MPD

You could play songs from MPD playlist :

    M-x dionysos-mpd-playlist

[MPD Playlist!](var/dionysos-mpd-0.3.png)

In this mode, you could use manage MPD :

Keybinding           | Description
---------------------|------------------------------------------------------------
<kbd>n</kbd>         | Go to the next song
<kbd>n</kbd>         | Go to the previous song
<kbd>c</kbd>         | Play song from current position
<kbd>s</kbd>         | Start playing
<kbd>SPC</kbd>       | Stop playing
<kbd>+</kbd>         | Raise volume
<kbd>-</kbd>         | Decrease volume

## Development

### Cask

``dionysos`` use [Cask][] for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

* Launch unit tests from shell:

        $ make clean test

* You could use [Overseer][] to launch unit tests from Emacs


## Support / Contribute

See [here](CONTRIBUTING.md)


## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>



[dionysos]: https://github.com/nlamirault/dionysos
[badge-license]: https://img.shields.io/badge/license-GPL_2-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/dionysos/blob/master/LICENSE
[Issue tracker]: https://github.com/nlamirault/dionysos/issues

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: http://melpa.milkbox.net/
[Cask]: http://cask.github.io/

[Overseer]: https://github.com/tonini/overseer.el
[vlc]: http://www.videolan.org/vlc/
[mplayer]: http://www.mplayerhq.hu/design7/news.html
