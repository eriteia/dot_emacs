
;; MenuBar Toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(desktop-save-mode 1)

;; Font
(set-default-font "Bitstream Vera Sans Mono-10")

;; Shell
(setq shell-file-name "C:/MinGW/msys/1.0/bin/bash")
(setq explicit-shell-file-name shell-file-name)
(setenv "PATH"
    (concat ".:/usr/local/bin:/mingw/bin:/bin:"
        (replace-regexp-in-string " " "\\\\ "
            (replace-regexp-in-string "\\\\" "/"
                (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
                                          (getenv "PATH"))))))

(defun shell-msys ()
  (interactive)
  (let ((explicit-sh.exe-args '("--login" "-i")))
    (shell-explicit "c:/MinGW/msys/1.0/bin/bash.exe" "Msys" ?/ nil)))

;; Hangul
(custom-set-variables
 '(default-input-method "korean-hangul3f"))

(prefer-coding-system 'utf-8)
(setq default-input-method "korean-hangul3f")
(setq default-korean-keyboard "3")
;;(set-language-environment "Korean")
;;(global-set-key (kbd "<kana>") 'toggle-input-method)
;;(global-set-key (kbd "<S-kana>") 'toggle-input-method)
;;(global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

;; load path
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/auto-java-complete")

;; +++++++++++++++++++++++++++++++++++++++++++
;; Load el files
;; +++++++++++++++++++++++++++++++++++++++++++
(load-file "~/.emacs.d/color-theme-tangotango.el")
(load-file "~/.emacs.d/sr-speedbar.el")
(load-file "~/.emacs.d/speedbar-extension.el")

;; ido
(require 'ido)
(ido-mode t)

;;
(speedbar-change-initial-expansion-list "buffers")
(global-set-key  [f8] 'speedbar-get-focus)

(require 'color-theme)
(color-theme-initialize)
(color-theme-tangotango)

;;; MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

;; Projectile
(projectile-global-mode)
(add-hook 'ruby-mode-hook 'projectile-mode)
;;(setq projectile-indexing-method 'alien)
(setq projectile-indexing-method 'native)

;;Auto-Completion
(require 'auto-complete-config)
(ac-config-default)
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x compile support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;; Helper function to find files. Source: emacswiki
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return 
the current directory"
  (let ((root (expand-file-name "/")))
    (loop for d = default-directory 
          then (expand-file-name ".." d)
          if (file-exists-p (expand-file-name file d))  return d
          if (equal d root) return nil)))

;; Tries to locate the gradlew wrapper, and if found create and return
;; a "make" string which changes into that directory and executes ./gradlew
;; with assembleDebug by default.
;;
;; How to recognize compilation errors.
;; For some reason, the errors returned when compilation is run from within emacs is:
;;   :TournmanApplication:compileRelease/home/marius/p/tournman/android/workspace/TournmanProject/TournmanApplication/src/main/java/net/kjeldahl/tournman/TournmanActivity.java:153: error: ';' expected
;;
;; This regexp captures the filename and line number by looking for ":compile.*?(filename):(lineno):
(require 'compile)

(defun gradleMake ()
  (unless (file-exists-p "gradlew")
    (set (make-local-variable 'compile-command)
         (let ((mkfile (get-closest-pathname "gradlew")))
           (if mkfile
               (progn (format "cd %s; ./gradlew assembleDebug"
                              mkfile))))))
  (add-to-list 'compilation-error-regexp-alist '(":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2)))

(add-hook 'java-mode-hook 'gradleMake)


;; WebMode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Default path
;;(setq default-directory (concat (getenv "HOME") "/"))
(setq default-directory "~/")

;;
;; Ruby
;;

;; inf-ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
    				     interpreter-mode-alist))
;;(autoload 'inf-ruby-keys "inf-ruby"
;;  "Set local key defs for inf-ruby in ruby-mode")
;;(add-hook 'ruby-mode-hook
;;          '(lambda ()
;;             (inf-ruby-keys)
;;	     ))

;; SmartParens
(require 'smartparens-config)

;; Yari key bind
(defun ri-bind-key () 
  (local-set-key [f1] 'yari-helm)) 
(add-hook 'ruby-mode-hook 'ri-bind-key)

;; Ruby tools
(require 'ruby-tools)

;; javascript indent
(setq js-indent-level 4)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(setq-default indent-tabs-mode nil)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
;;(add-to-list 'load-path "~/.emacs.d/org-mode/contrib" t)
(add-to-list 'load-path "~/.emacs.d/org-reveal")

;; ORG
(require 'ox-reveal)
(setq org-reveal-root "file:///c:/Users/swhong/git/reveal.js")
(setq org-image-actual-width nil)

;; Web beautify
(load-file "~/.emacs.d/web-beautify.el")
