# emacs-django-commands
Run django commands in inferior-python-mode

## Dependencies

Alongside with ```python``` and ```which-func``` which is shiped with GNU Emacs this package required [projectile](https://github.com/bbatsov/projectile).

## Installation

Put `django-commands.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'django-commands)
```

## Usage

Available commands:

* `django-shell`
* `django-server`
* `django-test`

With prefix arguments one can edit command argumets.

## Customize

```M-x customize-group RET django-commands RET```
