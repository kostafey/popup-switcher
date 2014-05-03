# popup-switcher

popup-switcher provides (yet another) convenient way to switch buffers, optional
navigation possibility through functions/methods and any other kind of switching
you like. It's easy to use and extend for custom purposes.  It uses popup lib
for better sense.

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

* `psw-switch-buffer` -- switch buffers through popup menu.
* `psw-switch-recentf` -- switch recent files.
* `psw-navigate-files` -- simple file navigator.
* `psw-switch-function` -- switch (navigate) through functions in the current
  buffer *(optional)*.
* `psw-switch-projectile-files` -- switch among projectile project files list
  *(optional)*.

Run <kbd>M-x psw-switch-buffer [RET]</kbd> (or your selected key).  Type some letters
from the name of buffer of interest (since isearch is enabled on start) to
filter buffers list, use arrow keys and <kbd>[RET]</kbd> or mouse to select
buffer.

<img src="https://dl.dropboxusercontent.com/u/820526/psw-switch-buffer.png" width="600" />
<img src="https://dl.dropboxusercontent.com/u/820526/psw-switch-function.png" width="600" />

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [popup](https://github.com/auto-complete/popup-el).
* [CEDET](http://cedet.sourceforge.net/) *(optional)*.
* [projectile](https://github.com/bbatsov/projectile) *(optional)*.
* [flx-ido](https://github.com/lewang/flx) *(optional)*.

## License

Copyright © 2013-2014 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 2.0+
