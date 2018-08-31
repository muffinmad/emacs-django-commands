# emacs-django-commands

This package allows to run django commands in GNU Emacs

## Installation

### Dependencies

Packages shipped with GNU Emacs: `compile`, `project`,  `python` and `which-func`

### With `package.el`

Download `django-commands.el` and run:
``` el
M-x package-install-file RET <path-to-django-commands-el> RET
```

### Without `package.el`

Put `django-commands.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'django-commands)
```

## Usage

### Interactive commands

#### `django-shell`

Runs shell command in `django-shell-mode`. It's derived from `inferior-python-mode` so there are native completions and pdb tracking mode.

#### `django-server`

Runs server command in comint-mode with pdb tracking mode and `compilation-shell-minor-mode`.

#### `django-test`

Asks test name to run and then runs test command in comint-mode with pdb tracking enabled and `compilation-shell-minor-mode`.

#### `django-restart`

Being runned in one of django-mode buffers restarts current django command.

### Commands arguments

If command is invoked with prefix argument (for ex. `C-u M-x django-shell <RET>`) it allow to edit command arguments.

## Customization

`M-x customize-group <RET> django-commands <RET>`

For each command you can specify command name and default arguments.

Also all variables can be customized through `dir-locals.el`. For example, you can specify python executable and django settings module for particular project:

``` el
((nil . ((django-commands-python-executable . "~/.virtualenvs/virtualenvname/bin/python")
         (django-commands-settings-module . "settings.module.name"))))
```

## Filtering django-server output

It may be useful to filter out some django server output. For example, don't output static files request and print out separator between server restarts:

``` el
(defun my/django-server-preoutput-filter (string)
  (cond ((string-match-p "^\\[[^]]+\\] .*\"GET /static/" string) "")
        ((string-prefix-p "Performing system checks...\n" string) (format "\f\n%s" string))
         (t string)))
(add-hook 'django-server-mode-hook
          #'(lambda()
              (page-break-lines-mode)
              (add-to-list 'comint-preoutput-filter-functions #'my/django-server-preoutput-filter)))
```

## Django shell and IPython

Native shell completions can't be enabled in IPython since IPython doesn't support readline. That's why `-i python` is default arguments for shell command.

Take a look at [rlipython](https://github.com/ipython/rlipython). If you decide to use it you can safely remove `-i python` from `django-commands-shell-args`.
