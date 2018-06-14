(require 'projectile)
(require 'python)
(require 'which-func)



;; Customizations

(defgroup django-commands nil
  "Settings for django-commands."
  :group 'tools
  :prefix "django-commands-")

(defcustom django-commands-manage-module "manage.py"
  "manage.py module"
  :type '(string))

(defcustom django-commands-settings-module nil
  "Default settings module.
If nil then DJANGO_SETTINGS_MODULE environment variable will be used."
  :type '(choice
          (const :tag "DJANGO_SETTINGS_MODULE" nil)
          (string)))

(defcustom django-commands-server-command "runserver"
  "runserver command"
  :type '(string))

(defcustom django-commands-server-args '("--nothreading" "127.0.0.1:8000")
  "runserver command default arguments"
  :type '(repeat string))

(defcustom django-commands-shell-command "shell_plus"
  "shell command"
  :type '(string))

(defcustom django-commands-shell-args '("--plain")
  "shell command default arguments"
  :type '(repeat string))

(defcustom django-commands-test-command "test"
  "test command"
  :type '(string))

(defcustom django-commands-test-args '("--keepdb" "--failfast")
  "test command default arguments"
  :type '(repeat string))

(defcustom django-commands-test-name-function #'django-test-name
  "Function to return name of a test to be run"
  :type 'function)



;; Modes

(define-derived-mode django-command-mode inferior-python-mode "Django command"
  "Major mode for django command"
  (setq mode-line-process '(":%s"))
  (setq-local django--current-command django-commands--current-command)
  (setq-local django--current-args django-commands--current-args)
  (setq-local django--current-comint-name django-commands--current-comint-name)
  (setq header-line-format '(:eval (let ((header-line (format "%s %s" django--current-command django--current-args)))
                                     (substring header-line (min (length header-line) (window-hscroll)))))))

(define-derived-mode django-shell-mode django-command-mode "Django shell"
  "Major mode for `django-shell'")

(defun django-server--skip-static (string)
  (if (string-match-p "GET /static-" string) "" string))

(defun django-server--clear-on-restart (string)
  (when (string-prefix-p "Performing system checks...\n" string)
	(let ((inhibit-read-only t))
	(erase-buffer)))
  string)

(define-derived-mode django-server-mode django-command-mode "Django server"
  "Major mode for `django-server'"
  (set (make-local-variable 'comint-preoutput-filter-functions)
	   '(django-server--skip-static
		 django-server--clear-on-restart)))

(define-derived-mode django-test-mode django-command-mode "Django test"
  "Major mode for `django-test'")



;; Funcs

(defun django-commands--buffer (mode comint-name)
  (if (or (not (derived-mode-p mode)) (comint-check-proc (current-buffer)))
	  (get-buffer-create (generate-new-buffer-name (concat "*" comint-name "*")))
    (progn
      (let ((inhibit-read-only t))
        (erase-buffer))
      (current-buffer))))

(defun django-commands--settings-args ()
  (let ((settings-module (or django-commands-settings-module (getenv "DJANGO_SETTINGS_MODULE"))))
    (if (string= "" settings-module)
        nil
      (list "--settings" settings-module))))

(defun django-commands--args (args)
  (let ((command-args (append (django-commands--settings-args) args)))
    (if current-prefix-arg
        (save-match-data (split-string (read-from-minibuffer "Args: " (mapconcat 'identity command-args " "))))
      command-args)))

(defun django-commands--command (command-name mode command args)
  (let* ((comint-name (concat (projectile-project-name) "-" command-name))
         (command-args (django-commands--args args))
         (buffer (django-commands--buffer mode comint-name)))
    (pop-to-buffer-same-window buffer)
    (setq default-directory (projectile-project-root))
    (let* ((buffer (apply 'make-comint-in-buffer comint-name buffer "python" nil (append (list django-commands-manage-module command) command-args)))
           (python-shell--interpreter "python")
           (python-shell--interpreter-args "-i")
           (django-commands--current-args (mapconcat 'identity command-args " "))
           (django-commands--current-command command)
           (django-commands--current-comint-name comint-name))
      (with-current-buffer buffer
        (funcall mode)))))

(defun django-test-name ()
  (let* ((module-name (save-match-data (split-string (file-relative-name (file-name-sans-extension buffer-file-name) (projectile-project-root)) "[/]")))
		 (func-name (which-function))
         (func-name (when func-name (list func-name))))
	(mapconcat 'identity (append module-name func-name) ".")))



;; Interactive funcs

(defun django-shell ()
  (interactive)
  (django-commands--command "shell" 'django-shell-mode django-commands-shell-command django-commands-shell-args))

(defun django-server ()
  (interactive)
  (django-commands--command "server" 'django-server-mode django-commands-server-command django-commands-server-args))

(defun django-test ()
  (interactive)
  (django-commands--command "test" 'django-test-mode django-commands-test-command (append django-commands-test-args (list (funcall django-commands-test-name-function)))))

(provide 'django-commands)
