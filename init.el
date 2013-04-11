(require 'package)
(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/") 
	("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir "^[^#].*el$")))

;; These are the last lines of my init.el. They will load any *.el
;; files in the ~/.emacs.d/users/user-login-name/ folder. Anything
;; specific for that machine goes there.

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when is-mac (require 'mac))
(require 'global)
;;(require 'scratch-backup)
(require 'bindings)

(load-theme 'solarized-dark t)
