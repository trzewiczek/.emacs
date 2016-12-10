(setq user-full-name    "Krzysztof Trzewiczek")
(setq user-mail-address "krzysztof@singup.pl")

;; packages management
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; take deft and magit from melpa
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; install default packages
(defvar singup/packages '(auto-complete 
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




;; put all backup and auto-save files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; start in clean *scratch* buffer with org-mode on
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; some gui tweaks
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(setq column-number-mode t)
(setq-default show-trailing-whitespace t)
(load-theme 'zenburn t)

;; intuitive selecting and copy/pasting text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; emapty line markers
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; yes or no questions shortened
(defalias 'yes-or-no-p 'y-or-n-p)

;; keybindings
(global-set-key (kbd "RET")   'newline-and-indent)
(global-set-key (kbd "C-;")   'comment-or-uncomment-region)
(global-set-key (kbd "M-/")   'hippie-expand)
(global-set-key (kbd "C-+")   'text-scale-increase)
(global-set-key (kbd "C--")   'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)


;; dialoging with emacs
(setq echo-keystrokes 0.1
      use-dialog-box  nil
      visible-bell    t)

;; highlight matching parens
(show-paren-mode t)


;; org-mode configuration
;; ----------------------

;; clean view
(setq org-startup-indented t)

;; english spelling and style
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'org-mode-hook (lambda () (writegood-mode)))

;; todo --> inprogress --> done
(setq org-todo-keywords      '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))

;; agenda settings
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/org"))
(setq org-startup-indented t)

;; add abbreviations to org-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
;; TODO define soem templates


;; deft configuration
;; ------------------
(setq deft-directory "~/org")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;; smex configuration
;; ------------------
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido configuration
;; -----------------
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; markdown mode
;; -------------
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
				(visual-line-mode t)
				(writegood-mode t)
				(flyspell-mode t)))



;; spellchecking
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq-default ispell-list-command "list")


;; and some more...
(require 'autopair)
(require 'auto-complete-config)
(ac-config-default)
