# README

My Emacs config

## Features
  * Relatively fast load time
  * Start in evil-mode by default
  * Well-setup for web development
  * Most things 'just work' out of the box

## Evil Keybindings

In command mode, standard navigation works:

j - Down a line

k - Up a line

h - left by a character

l - right by a character

... etc

Evil mode doesn't work in some buffers like magit diff, so standard Emacs / Unix navigation will work:

C-n Next line

C-p previous line

C-a beginning of line

C-e end of line

C-f next character

C-b previous character

... etc

## Notable keybindings

Defined in `lisp/personal-keybindings.el`

  * Say 'Nice to meet you': C-n-m
  * Say 'No problemo': C-c-n-p
  * Say 'Affirmative': C-c-y
  * Say 'Negative': C-c- -
  * Say "I've been waiting for you": C-c-w
  * Ace-jump: SPC
  * Lookup word in dictionary: C-c l


## Install (OS X)

* `brew install emacs --with-cocoa`
* `brew install cask`
* `git clone git@github.com:sufyanadam/.emacs-use-package ~/.emacs.d`
* `cd ~/.emacs.d`
* `cask install`

* Launch emacs
