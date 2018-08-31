;;; django-commands.el --- Run django commands inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: tools
;; URL: https://github.com/muffinmad/emacs-django-commands
;; Package-Version: 1.0

;;; Commentary:

;;; Code:

(require 'compile)
(require 'project)
(require 'python)
(require 'which-func)



;; Customizations

(defgroup django-commands nil
  "Settings for django-commands."
  :group 'tools
  :prefix "django-commands-")

(defcustom django-commands-python-executable "python"
  "Python executable."
  :type '(string))

(defcustom django-commands-manage-module "manage.py"
  "Name of a manage.py module."
  :type '(string))

(defcustom django-commands-settings-module nil
  "Default settings module.
If nil then DJANGO_SETTINGS_MODULE environment variable will be used."
  :type '(choice
          (const :tag "DJANGO_SETTINGS_MODULE" nil)
          (string)))

(defcustom django-commands-server-command "runserver"
  "Name of the runserver command."
  :type '(string))

(defcustom django-commands-server-args '("--nothreading" "127.0.0.1:8000")
  "Default arguments for runserver command."
  :type '(repeat string))

(defcustom django-commands-shell-command "shell"
  "Name of the shell command."
  :type '(string))

(defcustom django-commands-shell-args '("-i" "python")
  "Default arguments for shell command."
  :type '(repeat string))

(defcustom django-commands-test-command "test"
  "Name of the test command."
  :type '(string))

(defcustom django-commands-test-args '("--failfast" "--keepdb")
  "Default arguments for test command."
  :type '(repeat string))

(defcustom django-commands-test-name-function #'django-test-name
  "Function to return name of a test to be run."
  :type 'function)



;; Local vars

(defvar-local django-commands--current-args nil
  "Current command arguments")

;; Modes

(defun django-commands--clear-undo-output-filter (&optional _string)
  "Erase undo on process output."
  (setq buffer-undo-list nil))

(define-derived-mode django-shell-mode inferior-python-mode "Django shell"
  "Major mode for `django-shell'"
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions 'django-commands--clear-undo-output-filter t))

(define-derived-mode django-command-mode comint-mode "Django command"
  "Major mode for django command"
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions 'django-commands--clear-undo-output-filter t)
  (add-to-list 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function t)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-shell-compilation-regexp-alist)
  (compilation-shell-minor-mode 1))

(define-derived-mode django-server-mode django-command-mode "Django server"
  "Major mode for `django-server'")

(define-derived-mode django-test-mode django-command-mode "Django test"
  "Major mode for `django-test'")



;; Funcs

(defun django-commands--project-dir ()
  "Get project root directory."
  (or (cdr (project-current t)) (user-error "No project")))

(defun django-commands--project-name (project-dir)
  "Get project name based on PROJECT-DIR."
  (file-name-nondirectory (directory-file-name project-dir)))

(defun django-commands--mode-name (mode)
  "Get MODE name without '-mode' suffix."
  (substring (symbol-name mode) 0 -5))

(defun django-commands--buffer-name (mode project-dir)
  "Generate buffer name based on MODE and PROJECT-DIR."
  (format "*%s: %s*"
          (django-commands--mode-name mode)
          (django-commands--project-name project-dir)))

(defun django-commands--mode-buffer (mode buffer)
  "Return BUFFER if BUFFER can be used for MODE.  Return nil otherwise."
  (when (and buffer (with-current-buffer buffer (derived-mode-p mode)) (not (comint-check-proc buffer)))
    buffer))

(defun django-commands--buffer (mode project-dir)
  "Get buffer based on MODE and PROJECT-DIR.
Reuse current buffer or buffer with same name without living process otherwise create new buffer."
  (or (django-commands--mode-buffer mode (current-buffer))
      (let ((buffer-name (django-commands--buffer-name mode project-dir)))
        (django-commands--mode-buffer mode (get-buffer buffer-name))
        (generate-new-buffer buffer-name))))

(defun django-commands--settings-args ()
  "Get settings module as argument."
  (let ((settings-module (or django-commands-settings-module (getenv "DJANGO_SETTINGS_MODULE"))))
    (when (and settings-module (not (string= "" settings-module)))
      (list "--settings" settings-module))))

(defun django-commands--mode-args (mode)
  "Get command args based on MODE."
  (cond ((eq mode 'django-server-mode) django-commands-server-args)
        ((eq mode 'django-shell-mode) django-commands-shell-args)
        ((eq mode 'django-test-mode) django-commands-test-args)
        (t (error "No django command args for mode: %S" (symbol-name mode)))))

(defun django-commands--args (mode &optional args)
  "Get arguments for MODE with optional ARGS."
  (append (django-commands--settings-args) (django-commands--mode-args mode) args))

(defun django-commands--confirm-args (args)
  "Confirm ARGS if `current-prefix-arg'."
  (if current-prefix-arg
      (save-match-data (split-string (read-from-minibuffer "Args: " (mapconcat 'identity args " "))))
    args))

(defun django-commands--mode-command (mode)
  "Get command to execute based on MODE."
  (cond ((eq mode 'django-server-mode) django-commands-server-command)
        ((eq mode 'django-shell-mode) django-commands-shell-command)
        ((eq mode 'django-test-mode) django-commands-test-command)
        (t (error "No django command for mode: %S" (symbol-name mode)))))

(defun django-commands--kill-current-process (buffer)
  "Return t if new process can be started in BUFFER."
  (let ((process (get-buffer-process buffer)))
    (if process
        (and (yes-or-no-p "Kill current process?") (or (delete-process process) t))
      t)))

(defvar python-shell--interpreter "python")
(defvar python-shell--interpreter-args "-i")

(defun django-commands--run-command (buffer mode args)
  "Run command based on MODE with ARGS in BUFFER."
  (when (django-commands--kill-current-process buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (hack-dir-local-variables-non-file-buffer)
      (apply #'make-comint-in-buffer (django-commands--mode-name mode) buffer django-commands-python-executable nil (append (list django-commands-manage-module (django-commands--mode-command mode)) args))
      (funcall mode)
      (setq
       list-buffers-directory (abbreviate-file-name default-directory)
       django-commands--current-args (mapconcat 'identity args " "))
      (setq
       header-line-format '(:eval (let ((header-line (format "Args: %s" django-commands--current-args)))
                                    (substring header-line (min (length header-line) (window-hscroll)))))))))

(defun django-commands--command (mode &optional args)
  "Run command based on MODE with optional ARGS."
  (let* ((project-dir (django-commands--project-dir))
         (args (with-temp-buffer
                 (setq default-directory project-dir)
                 (hack-dir-local-variables-non-file-buffer)
                 (django-commands--confirm-args (django-commands--args mode args))))
         (buffer (django-commands--buffer mode project-dir)))
    (with-current-buffer buffer
      (setq default-directory project-dir))
    (django-commands--run-command buffer mode args)
    (pop-to-buffer buffer)))



;; Interactive funcs

;;;###autoload
(defun django-shell ()
  "Run shell command.
If run with universal argument allow to edit command arguments"
  (interactive)
  (django-commands--command #'django-shell-mode))

;;;###autoload
(defun django-server ()
  "Run server command.
If run with universal argument allow to edit command arguments"
  (interactive)
  (django-commands--command #'django-server-mode))

;;;###autoload
(defun django-test ()
  "Ask for test name and run test."
  (interactive)
  (django-commands--command #'django-test-mode (save-match-data (split-string (read-from-minibuffer "Test name: " (funcall django-commands-test-name-function))))))

;;;###autoload
(defun django-restart ()
  "Restart django command associated with buffer.
If run with universal argument allow to edit command arguments"
  (interactive)
  (unless (derived-mode-p 'django-command-mode 'django-shell-mode)
    (user-error "No django command in this buffer"))
  (django-commands--run-command (current-buffer) major-mode (django-commands--confirm-args (save-match-data (split-string django-commands--current-args)))))

;;;###autoload
(defun django-test-name ()
  "Return name of test case to run."
  (let ((project-dir (cdr (project-current))))
    (when (and project-dir buffer-file-name)
      (let* ((module-name (save-match-data (split-string (file-relative-name (file-name-sans-extension buffer-file-name) project-dir) "/")))
             (func-name (which-function))
             (func-name (when func-name (list func-name))))
        (mapconcat 'identity (append module-name func-name) ".")))))

(provide 'django-commands)

;;; django-commands.el ends here
