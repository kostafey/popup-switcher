[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)
[![MELPA](http://melpa.org/packages/popup-switcher-badge.svg)](http://melpa.org/#/popup-switcher)

# popup-switcher

popup-switcher provides (yet another) convenient way to switch buffers, optional
navigation possibility through functions/methods and any other kind of switching
you like. It's easy to use and extend for custom purposes.  It uses popup lib
for better sense.

- [Installation](#installation)
- [Configuration](#configuration)
  - [Keybindings](#keybindings)
  - [Maximum number of visible items](#maximum-number-of-visible-items)
  - [Menu position](#menu-position)
  - [Fuzzy matching](#fuzzy-matching)
  - [Modified buffers marker](#modified-buffers-marker)
  - [Functions/methods navigation](#functionsmethods-navigation)
- [Usage](#usage)
- [Requirements](#requirements)
- [License](#license)

## Installation

Add MELPA (if not yet) to your `package-archives` list:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

Then you can install popup-switcher with the following command:

<kbd>M-x package-install [RET] popup-switcher [RET]</kbd>

Load popup-switcher:

```lisp
(require 'popup-switcher)
```

## Configuration

### Keybindings

Add a hotkey to `psw-switch-<X>` to you taste (for the full list of predefined
switching functions see
[Usage](https://github.com/kostafey/popup-switcher#usage) section), e.g.:

```lisp
(global-set-key [f2] 'psw-switch-buffer)
```

### Maximum number of visible items

Set maximum number of visible items in popup menus

```lisp
(setq psw-popup-menu-max-length 15)
```

### Menu position

Non-nil `psw-in-window-center` means horizontal locate popup menu in the window
center.  Locate popup menu in the `fill-column` center otherwise (by default).

```lisp
(setq psw-in-window-center t)
```

### Fuzzy matching

Non-nil `psw-use-flx` enables `flx` fuzzy matching engine for isearch in popup
menus. [flx-ido](https://github.com/lewang/flx) is required in this case, it can
be installed by your favorite approach. E.g. by `MEPLA`:
<kbd>M-x package-install [RET] flx-ido [RET]</kbd>

```lisp
(setq psw-use-flx t)
```

### Modified buffers marker

Non-nil `psw-mark-modified-buffers` means mark modified buffers with star char
(`*`) expect special beffers like `*Messages*` any time you call
`psw-switch-buffer`.

### Functions/methods navigation

To navigate through functions/methods names you should add the most recent
`CEDET` and load `eassist` lib. This feature is optional, and popup-switcher can
operate without `CEDET` (and therefore without navigation possibility through
functions/methods).

```lisp
;; Activate semantic
(semantic-mode 1)

;; Load contrib library
(add-to-list 'load-path "~/.emacs.d/cedet/contrib/")
(require 'eassist)

(eval-after-load "eassist"
  '(global-set-key [f3] 'psw-switch-function))
```

## Usage

List of interactive functions:

 Command                       | Description
-------------------------------|------------------------------------------
 `psw-switch-buffer`           | switch buffers through popup menu
 `psw-switch-recentf`          | switch recent files
 `psw-navigate-files`          | simple file navigator
 `psw-switch-function`         | switch (navigate) through functions in the current buffer *(optional)*
 `psw-switch-projectile-files` | switch among projectile project files list *(optional)*

Run <kbd>M-x psw-switch-buffer [RET]</kbd> (or your selected key).  Type some letters
from the name of buffer of interest (since isearch is enabled on start) to
filter buffers list, use arrow keys and <kbd>[RET]</kbd> or mouse to select
buffer.

When you are in menu created by `psw-switch-buffer`, you can kill selected
buffer by pressing <kbd>C-d</kbd> or <kbd>C-k</kbd>.

<img src="https://dl.dropboxusercontent.com/u/820526/psw-switch-buffer.png" width="600" />
<img src="https://dl.dropboxusercontent.com/u/820526/psw-switch-function.png" width="600" />

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [popup](https://github.com/auto-complete/popup-el).
* [CEDET](http://cedet.sourceforge.net/) *(optional)*.
* [projectile](https://github.com/bbatsov/projectile) *(optional)*.
* [flx-ido](https://github.com/lewang/flx) *(optional)*.

## License

Copyright © 2013-2015 Kostafey <kostafey@gmail.com> and
[contributors](https://github.com/kostafey/popup-switcher/contributors)

Distributed under the General Public License 2.0+
