;; MenuBar Toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)

;; Font
(set-default-font "Bitstream Vera Sans Mono-9")

;; Hangul
(custom-set-variables
 '(default-input-method "korean-hangul3f"))

;; load path
(add-to-list 'load-path "~/.emacs.d/color-theme")

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

