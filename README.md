[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)
[![MELPA](http://melpa.org/packages/popup-switcher-badge.svg)](http://melpa.org/#/popup-switcher)

# popup-switcher

popup-switcher provides (yet another) convenient way to switch buffers,
navigation possibility through functions/methods and any other kind of switching
you like. It's easy to use and extend for custom purposes.  It uses popup lib
for better sense.

- [Installation](#installation)
- [Configuration](#configuration)
  - [Keybindings](#keybindings)
  - [Functions/methods navigation](#functionsmethods-navigation)
  - [Maximum number of visible items](#maximum-number-of-visible-items)
  - [Menu position](#menu-position)
  - [Fuzzy matching](#fuzzy-matching)
  - [Modified buffers marker](#modified-buffers-marker)
- [Usage](#usage)
- [Screenshots](#Screenshots)
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

### Functions/methods navigation

Uses imenu to navigate through functions/methods names via
`psw-switch-function`.

```lisp
(global-set-key [f3] 'psw-switch-function)
```

### Maximum number of visible items

Set maximum number of visible items in popup menus

```lisp
(setq psw-popup-menu-max-length 15)
```

### Menu position
Menu opens centered with respect to `fill-column` by default.
Set `psw-popup-position` variable to change horizontal positioning.

Possible values are:

* `fill-column` - center relative to fill-column (default setting)
* `center` - center relative to window borders
* `point` - open popup at point

```lisp
(setq psw-popup-position 'center)
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

### Enter to dired-mode via psw-navigate-files

```lisp
(setq psw-enable-single-dot-to-navigate-files t)
```
Any time you run `psw-navigate-files` fn you can select dot `.`
item, which opens `dired-mode` for current directory.

### Highlight previous buffer for psw-switch-buffer

If you want to highlight previous buffer for `psw-switch-buffer` fn
set `psw-highlight-previous-buffer` to `t`:
```lisp
(setq psw-highlight-previous-buffer t)
```

## Usage

List of interactive functions:

 Command                          | Description
----------------------------------|------------------------------------------
 `psw-switch-buffer`              | switch buffers through popup menu
 `psw-switch-recentf`             | switch recent files
 `psw-navigate-files`             | simple file navigator
 `psw-switch-function`            | switch (navigate) through functions in the current buffer *(optional)*
 `psw-switch-projectile-files`    | switch among projectile project files list *(optional)*
 `psw-switch-projectile-projects` | switch among projectile projects list and it's files *(optional)*

Run <kbd>M-x psw-switch-buffer [RET]</kbd> (or your selected key).  Type some letters
from the name of buffer of interest (since isearch is enabled on start) to
filter buffers list, use arrow keys and <kbd>[RET]</kbd> or mouse to select
buffer.

When you are in menu created by `psw-switch-buffer`, you can kill selected
buffer by pressing <kbd>C-d</kbd> or <kbd>C-k</kbd>.

## Screenshots

Switch buffer (<code>psw-switch-buffer</code>)<br/>
<img src="https://user-images.githubusercontent.com/1282079/71625114-cb39e000-2bee-11ea-95b8-afa74341a6cf.png" width="600" />
<br/>Switch function (<code>psw-switch-function</code>)<br/>
<img src="https://user-images.githubusercontent.com/1282079/71625295-b3169080-2bef-11ea-8860-3653cce62ca0.png" width="600" />
<br/>Fuzzy matching for switch function<br/>
<img src="https://user-images.githubusercontent.com/1282079/71626022-aac05480-2bf3-11ea-8e51-fd081da3b7db.png" width="600" />
<br/>File navigator (<code>psw-navigate-files</code>)<br/>
<img src="https://user-images.githubusercontent.com/1282079/71625459-a0508b80-2bf0-11ea-811e-49f2787b7ce1.png" width="600" />

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [popup](https://github.com/auto-complete/popup-el).
* [projectile](https://github.com/bbatsov/projectile) *(optional)*.
* [flx-ido](https://github.com/lewang/flx) *(optional)*.

## License

Copyright © 2013-2019 Kostafey <kostafey@gmail.com> and
[contributors](https://github.com/kostafey/popup-switcher/contributors)

Distributed under the General Public License 2.0+
