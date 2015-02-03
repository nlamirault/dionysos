# dionysos

[![travis][badge-travis]][travis]
[![drone][badge-drone]][drone]
[![Melpa Status](http://melpa.milkbox.net/packages/dionysos-badge.svg)](http://melpa.milkbox.net/#/dionysos)
[![MELPA Stable](http://stable.melpa.org/packages/dionysos-badge.svg)](http://stable.melpa.org/#/dionysos)
[![Coverage Status](https://coveralls.io/repos/nlamirault/dionysos/badge.png)](https://coveralls.io/r/nlamirault/dionysos)

`dionysos` is a simple music player for Emacs.
Backends available are : [vlc][], [mplayer][]

## Installation

The recommended way to install ``dionysos`` is via [MELPA][]:

    M-x package-install dionysos

or [Cask][]:

	(depends-on "dionysos")

## Usage

### Backend

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
[travis]: https://travis-ci.org/nlamirault/dionysos
[badge-travis]: http://img.shields.io/travis/nlamirault/dionysos.svg?style=flat
[badge-drone]: https://drone.io/github.com/nlamirault/dionysos/status.png
[drone]: https://drone.io/github.com/nlamirault/dionysos/latest
[Issue tracker]: https://github.com/nlamirault/dionysos/issues

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: http://melpa.milkbox.net/
[Cask]: http://cask.github.io/

[Overseer]: https://github.com/tonini/overseer.el
[vlc]: http://www.videolan.org/vlc/
[mplayer]: http://www.mplayerhq.hu/design7/news.html
