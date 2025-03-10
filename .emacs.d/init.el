;; This file was generated from an org-mode file. Please make your
;; changes there and compile it afterwards.

(defvar ludwigd/default-font-family "JetBrains Mono")
(defvar ludwigd/default-font-size 120)
(defvar ludwigd/default-color-theme 'leuven-dark)
(defvar ludwigd/default-line-numbers-type 'relative)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)			; Disable the tool bar
(menu-bar-mode -1)			; Disable the menu bar
(scroll-bar-mode -1)			; Disable visible scroll bar
(set-fringe-mode 10)			; Give some breathing room
(setq frame-title-format "%b")	; Shorten the frame title

;; Set a nice theme
(if ludwigd/default-color-theme
    (load-theme ludwigd/default-color-theme))

(global-set-key [f12] 'theme-choose-variant)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(column-number-mode 1)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type ludwigd/default-line-numbers-type)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-fontset-font "fontset-default"
                  'ascii ludwigd/default-font-family)

(set-face-attribute 'default nil
                :font "fontset-default"
                :height ludwigd/default-font-size)

(setq current-fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  "Open a new line before the current one. 
   See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config (setq aw-dispatch-always t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
     :map ivy-minibuffer-map
     ("TAB" . ivy-alt-done)
     ("C-l" . ivy-alt-done)
     ("C-j" . ivy-next-line)
     ("C-k" . ivy-previous-line)
     :map ivy-switch-buffer-map
     ("C-k" . ivy-previous-line)
     ("C-l" . ivy-done)
     ("C-d" . ivy-switch-buffer-kill)
     :map ivy-reverse-i-search-map
     ("C-k" . ivy-previous-line)
     ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("C-c j" . 'counsel-git-grep)
     :map minibuffer-local-map
     ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :config (setq magit-define-global-key-bindings t))

(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . TeX-source-correlate-mode)
  :config
  (add-to-list 'TeX-view-program-list '("mu-pdf" ("/usr/bin/mupdf" " %o" (mode-io-correlate " %(outpage)"))))
  (add-to-list 'TeX-view-program-list '("zathura" ("/usr/bin/zathura" " %o" (mode-io-correlate " %(outpage)"))))
  (setq TeX-view-program-selection '((output-pdf "zathura"))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

(require 'sh-script)                    ; For sh-mode-map

(defun winny--extract-shellcheck-error (err)
  (and-let* (((eq (flycheck-error-checker err) 'sh-shellcheck)))
    (flycheck-error-id err)))

(defun winny/shellcheck-disable-at-line ()
  "Insert \"# shellcheck disable=SC...\" line to silence shellcheck errors."
  (interactive)
  (save-match-data
    (save-excursion
      (and-let* ((errs
                  (cl-loop for err in (flycheck-overlay-errors-in (pos-bol) (pos-eol))
                           if (winny--extract-shellcheck-error err)
                           collect (winny--extract-shellcheck-error err))))
        (beginning-of-line)
        (insert (format "# shellcheck disable=%s"
                        (mapconcat 'identity errs ",")))
        (indent-according-to-mode)
        (newline-and-indent)))))

(add-hook 'sh-mode-hook
          (defun winny--bind-shellcheck-disable ()
            (define-key sh-mode-map (kbd "C-c ! k") 'winny/shellcheck-disable-at-line)))
