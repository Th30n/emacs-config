# My Emacs configuration

This is my starting Emacs configuration. It will probably expand as I am learning
Emacs.
Currently there are (very basic) configuration for the following programming
languages:

  * Haskell
  * Common Lisp (via Slime)
  * C# (via Omnisharp)

## Installation

Remove and **backup** your `~/.emacs.d` directory then follow these steps:

  * Clone repo `git clone https://github.com/Th30n/emacs-config ~/.emacs.d`
  * You will probably want to merge your backup without overwriting cloned files.
  * After running Emacs, missing packages will automatically be downloaded.

## Configuration structure

Package and mode configurations are in `~/.emacs.d/lisp/init-*.el` files and
they will automatically download required packages. If a certain configuration
isn't needed comment out the `(require 'init-*)` inside `~/.emacs.d/init.el`.

## Inspiration

Although currently very basic, main inspiration for the structure of
configuration files is [Purcell's config](https://github.com/purcell/emacs.d).
