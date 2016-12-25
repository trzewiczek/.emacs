(setq user-full-name    "Krzysztof Trzewiczek")
(setq user-mail-address "krzysztof@singup.pl")

;; packages management
;; ----------------------------------------------------------------------------
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; selected packeges take from melpa
(setq package-archive-enable-alist '(("melpa" deft magit yasnippet)))

;; install default packages
(defvar singup/packages
  '(auto-complete
    autopair
    deft
    expand-region
    flycheck
    htmlize
    magit
    magithub
    markdown-mode
    marmalade
    org
    python
    rainbow-mode
    smex
    solarized-theme
    volatile-highlights
    writegood-mode
    yasnippet
    zenburn-theme)
  "List of default packages")

(defun singup/packages-installed-p ()
  (cl-every 'package-installed-p singup/packages))

(unless (singup/packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p singup/packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; backup fails and autosaves
;; ----------------------------------------------------------------------------
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; clean starup into org mode
;; ----------------------------------------------------------------------------
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


;; gui tweaks
;; ----------------------------------------------------------------------------
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(setq column-number-mode t)

;; estetics
(load-theme 'zenburn t)
(set-default-font "Inconsolata-11")

;; highligh empty lines and trailing whitespaces
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(setq-default show-trailing-whitespace t)

;; indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; highlight matching parens
(show-paren-mode t)


;; dialoging with emacs
;; ----------------------------------------------------------------------------
(setq echo-keystrokes 0.1
      use-dialog-box  nil
      visible-bell    t)

;; yes or no questions shortened
(defalias 'yes-or-no-p 'y-or-n-p)


;; intuitive selecting and copy/pasting
;; ----------------------------------------------------------------------------
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)


;; keybindings
;; ----------------------------------------------------------------------------
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; org-mode agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; smaex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; org-mode configuration
;; ----------------------------------------------------------------------------
(defvar org-mode-env "~/org")

;; clean view
(setq org-startup-indented t)

;; english spelling and style
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'org-mode-hook (lambda () (writegood-mode)))

;; todo --> inprogress --> done
(setq org-todo-keywords      '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "#73aed6" :weight bold))))

;; agenda settings
(setq org-agenda-files (cons org-mode-env '()))
(setq org-startup-indented t)


;; yasnippet configuration
;; ----------------------------------------------------------------------------
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)


;; deft configuration
;; ----------------------------------------------------------------------------
(setq deft-directory 'org-mode-env)
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)


;; smex configuration
;; ----------------------------------------------------------------------------
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)


;; ido configuration
;; ----------------------------------------------------------------------------
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;; markdown configuration
;; ----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
				(visual-line-mode t)
				(writegood-mode t)
				(flyspell-mode t)))


;; flyspell configuration
;; ----------------------------------------------------------------------------
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq-default ispell-list-command "list")
(setq ispell-dictionary "polish")

;; misc things
;; ----------------------------------------------------------------------------
(require 'autopair)
(require 'auto-complete-config)
(ac-config-default)
