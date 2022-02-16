;; disable lazy load of use-package?
;; (setq use-package-always-demand (daemonp))
(if (daemonp)
    (setq use-package-always-demand t))

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
   '(lsp-ui cargo elm-mode use-package vterm ddskk lsp-mode racer markdown-preview-mode haskell-mode ctags-update company auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load path
(add-to-list 'load-path "/Users/ryoh/.emacs.d/auto-capitalize/")
(add-to-list 'load-path "/Users/ryoh/.emacs.d/rust-mode/")
;;(add-to-list 'load-path "/Users/ryoh/.emacs.d/elm-mode/")

;; --- use package ---

;; Elm mode
(use-package elm-mode
  :commands (elm-mode)
  )

;; Rust mode
(use-package rust-mode
  :commands (rust-mode)
  )
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; auto capitalize
(use-package auto-capitalize
  :commands auto-capitalize-mode
  :init
  (add-hook 'org-mode-hook 'auto-capitalize-mode)
  )
(add-hook 'org-mode-hook 'auto-capitalize-mode)


;; Hooks
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'skk-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; hook c-mode when .ino extension
(add-to-list 'auto-mode-alist
             '("\\.ino\\'" . (lambda ()
                               ;; add major mode setting here, if needed, for example:
                               (c-mode)
			       )))

;; ------ Misc ------
;; disable warning of cl
(setq byte-compile-warnings '(cl-functions))

;; Skip startup screen
(setq inhibit-startup-screen t)

(ac-config-default)

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

;; Scheme
(set-variable (quote scheme-program-name) "stk")

;; markdown mode
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

;; --- For writings in Japanese ---

;; 句読点を右下に
(set-language-environment "Japanese")

;; ddssk
;; (setq-default skk-kutouten-type 'jp)
(define-key minibuffer-local-map (kbd "C-j") 'skk-kakutei)

;; " "をsticky shiftに用いることにする
(setq skk-sticky-key " ")

;; zSPC inserts space
(setq skk-rom-kana-rule-list '(("z " nil " ")))

;; --- For writings in English --- 

;; set dictionary english
(with-eval-after-load "ispell"
  (setq ispell-local-dictionary "en_US")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; double sapce into period
(defun freaky-space ()
  (interactive)
  (cond ((looking-back "\\(?:^\\|\\.\\)  +")
         (insert " "))
        ((eq this-command
             last-command)
         (backward-delete-char 1)
         (insert ". "))
        (t
         (insert " "))))
(define-key text-mode-map " " 'freaky-space)

;; --- Try ---

;;https://keens.github.io/blog/2020/12/01/rustnokankyoukouchiku_emacs_/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #rust

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)


(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lsp

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :hook (elm-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)
