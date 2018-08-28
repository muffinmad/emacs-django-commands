# emacs-django-commands
Run django commands in GNU Emacs

## Dependencies

Packages shiped with GNU Emacs:

* ```compile```
* ```project```
* ```python```
* ```which-func```

## Installation

### With `package.el`

Download `django-commands.el` and run from Emacs:
``` el
M-x package-install-file RET <path-to-django-commands-el> RET
```

### Without `package.el`

Put `django-commands.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'django-commands)
```

## Usage

Available commands:

* `django-server`
* `django-shell`
* `django-test`
* `django-restart`

With prefix arguments one can edit command argumets.

## Customize

```M-x customize-group RET django-commands RET```

## IPython

Since ipython doesn't support readline native shell completions can't be enabled in django-shell-mode and thats why ```-i python``` in the default shell command arguments.
Take a look at [rlipython](https://github.com/ipython/rlipython). If you decide to use it you can safely remove ```-i python``` from ```django-commands-shell-args```.
