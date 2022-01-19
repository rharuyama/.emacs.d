;;(package-initialize)

;; Install MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "/Users/ryoh/.emacs.d/use-package")
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(ddskk-posframe use-package vterm ddskk lsp-mode racer markdown-preview-mode haskell-mode ctags-update company auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load path
(add-to-list 'load-path "/Users/ryoh/.emacs.d/auto-capitalize/")
(add-to-list 'load-path "/Users/ryoh/.emacs.d/rust-mode/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; (use-package package
;;   :config
;;   (add-to-list 'package-archives
;; 	       '("melpa" . "https://melpa.org/packages/") t))

(when (require 'package nil t)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t))

;;
(ac-config-default)

;; use auto capitalize in org mode
;;(require 'auto-capitalize)
(use-package auto-capitalize
  :init
  (add-hook 'org-mode-hook 'auto-capitalize-mode)
  )

;; Show line number
;; (require 'linum)
;; (global-linum-mode 1)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'skk-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;;(add-hook 'org-mode-hook 'auto-capitalize-mode)

;; ------ Misc ------
;; Skip startup screen
(setq inhibit-startup-screen t)

(ac-config-default)
(add-hook 'org-mode-hook 'auto-capitalize-mode)

;; Highlighting brackets
(show-paren-mode 1)

;; Disable *.~ backup files
(setq make-backup-files nil)

;; Enable autopairing brackets
(electric-pair-mode 1)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Bind cmd to super
;(setq mac-command-modifier 'super)

;; Save cursor place
(save-place-mode 1)

;; Disable beep sound
(setq ring-bell-function 'ignore)

;; Change font size
(set-face-attribute 'default nil :height 140)

;; Show scroll bar
(scroll-bar-mode -1)
;; (set-scroll-bar-mode 'right)

;; Enable case-sensitivity
(setq case-fold-search nil)

;; Rust mode
(require 'rust-mode)
(autoload 'rust-mode "rust-mode" nil t)
;; Rust autocomplete
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Scheme
(set-variable (quote scheme-program-name) "stk")

;; Haskell mode
;;(require 'package)

;(package-initialize)

;; company-mode (for autocomplete)
;;(add-hook 'after-init-hook 'global-company-mode)

;; markdown mode
;;(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;(package-initialize)
;; change color
(setq markdown-preview-stylesheets (list "github.css"))

;; set environment for perl
(setenv "LC_ALL" "C")

;; replaces forward-sentence
(global-set-key (kbd "M-n")
  (lambda ()
    (interactive)
    (setq this-command 'forward-line)
    (forward-line 5)))

;; replaces backward-sentence
(global-set-key (kbd "M-p")
  (lambda ()
    (interactive)
    (setq this-command 'forward-line)
    (forward-line -5)))

;; autocomplete
(ac-config-default)

;; --- For writings in general ---

;; enable org mode export to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; --- For writings in Japanese---
;;(global-set-key (kbd "C-x C-j") 'skk-mode)

;; 日本語を補完しない
;; (defun edit-category-table-for-company-dabbrev (&optional table)
;;   (define-category ?s "word constituents for company-dabbrev" table)
;;   (let ((i 0))
;;     (while (< i 128)
;;       (if (equal ?w (char-syntax i))
;;       (modify-category-entry i ?s table)
;;     (modify-category-entry i ?s table t))
;;       (setq i (1+ i)))))
;; (edit-category-table-for-company-dabbrev)
;; (add-hook 'TeX-mode-hook 'edit-category-table-for-company-dabbrev) ; 下の追記参照
(setq company-dabbrev-char-regexp "\\cs")

;; 句読点を右下に
(set-language-environment "Japanese")

;; ddssk
;; (setq-default skk-kutouten-type 'jp)

;; enable org mode export to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; eshell disable unix command emulation
(eval-after-load "esh-module"
    '(setq eshell-modules-list (delq 'eshell-ls (delq 'eshell-unix eshell-modules-list))))

;; " "をsticky shiftに用いることにする
(setq skk-sticky-key " ")

;; --- For writings in English --- 

;; set dictionary english
(with-eval-after-load "ispell"
  (setq ispell-local-dictionary "en_US")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; enable latex jarticle 
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("jarticle"
               "\\documentclass{jarticle}"
               ("\\section{%s}" . "\\section*{%s}")))

(setq org-latex-pdf-process
      '("ptex2pdf -l -o \"-synctex=1 -file-line-error\" %f"
        "bibtex %b"))

;; default frame size
;; (add-to-list 'default-frame-alist '(height . 48))
;; (add-to-list 'default-frame-alist '(width . 150))

;;
(define-key minibuffer-local-map (kbd "C-j") 'skk-kakutei)

;; run ansi-term on startup
;; (setq initial-buffer-choice #'ansi-term)
;; (setq initial-buffer-choice (lambda () (term "zsh")))
