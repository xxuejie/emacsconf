;; recursively add load path
(let ((default-directory "~/.emacs.d/lisps/"))
  (normal-top-level-add-to-load-path '("."
                                       "color-theme-6.6.0"
                                       "emacs-color-theme-solarized"
                                       "clojure-mode"
                                       "ack-and-a-half"
                                       "yasnippet"
                                       "code-imports"
                                       "lua-mode"
                                       "zencoding"
                                       "lusty-emacs"
                                       "web-mode"
                                       "yaml-mode"
                                       "coffee-mode"
                                       "emacs-slim"
                                       "haml-mode"
                                       "less-css-mode"
                                       "sass-mode"
                                       "scss-mode"
                                       "ace-jump-mode"
                                       "javadoc-lookup"
                                       "highlight-indent"
                                       "handlebars-mode"
                                       "markdown-mode"
                                       "rainbow-delimiters")))

;; ido mode
(require 'ido)
(ido-mode t)

;; fix exec-path on mac
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; no splash screen
(setq inhibit-splash-screen t)

;; no tabs
(setq-default indent-tabs-mode nil)

;; set default indent to 2 spaces
(setq standard-indent 2)

;; set tab size to 4 spaces for dealing with windows-related projects
(setq-default tab-width 4)

;; no scroll bars, menu bars, tool bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not window-system)
  (menu-bar-mode -1))

;; show matching parentheses
(show-paren-mode t)

;; show column number
(column-number-mode t)

;; which function mode
(which-function-mode t)

(require 'highlight-indentation)

;; windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; use current line when no text is selected
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; tramp mode
(require 'tramp)
(setq tramp-default-method "ssh")

;; clone file into other buffer
(global-set-key (kbd "C-c c") 'clone-indirect-buffer-other-window)

;; builtin header switch
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; file backup settings
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 2
 kept-old-versions 1
 version-control t)       ; use versioned backups

;; stores auto save files in temp directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ;; color theme
;; (require 'color-theme)
;; (color-theme-initialize)

;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisps/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; font setting
(set-default-font "Dejavu Sans Mono for Powerline-14")

;; clojure mode
(require 'clojure-mode)

;; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)

;; ack-and-a-half
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;;;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; yasnippet
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/lisps/yasnippet-snippets"))
(yas/global-mode 1)

;; protobuf mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto?$" . protobuf-mode))

;; hs-minor-mode
(defun hs-minor-hook ()
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t))
(add-hook 'c-mode-common-hook 'hs-minor-hook)
(add-hook 'java-mode-hook 'hs-minor-hook)

;; highlight symbol
(require 'highlight-symbol)
(global-set-key (kbd "C-c M-h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c M-c") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-c M-n") 'highlight-symbol-next)
(global-set-key (kbd "C-c M-p") 'highlight-symbol-prev)
(global-set-key (kbd "C-c M-q") 'highlight-symbol-query-replace)

;; code-imports
(require 'code-imports)

;; google c style
(require 'google-c-style)
(defun google-c-style-hook()
  (google-set-c-style)
  (c-set-style "google"))

;; add microsoft c style
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (statement-cont . 0)
                (innamespace . [4])
                (inline-open . 0)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (template-args-cont . +))))

;; match c styles according to directory
(setq c-style-variables-are-local-p t)

(defun microsoft-c-style-hook ()
  (c-set-style "microsoft")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode t))

(defun per-project-c-style-mode-hook ()
  (let ((bname (buffer-file-name)))
    (cond
     ((string-match "clreflect" bname) (microsoft-c-style-hook))
     (t (google-c-style-hook)))))
(add-hook 'c-mode-common-hook 'per-project-c-style-mode-hook)

;; find closed makefile
(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
		  (if startdir startdir ".")))
	(found nil) ; found is set as a flag to leave loop if we find it
	(top nil))  ; top is set when we get
		    ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
	  (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
	  (setq found t)
	; If not, move up a directory
	(setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found dirname nil)))

(defun compile-closest-makefile ()
  (interactive)
  (let* ((default-directory (or (upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))

;; lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; cmake mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; uses python mode for pidbile
(add-to-list 'auto-mode-alist '("\\.pibfile$" . python-mode))

;; ruby files
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; paredit mode with rainbow delimiters for lisp files
(defun paredit-with-rainbow-hook ()
  (paredit-mode +1)
  (rainbow-delimiters-mode))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(require 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook       'paredit-with-rainbow-hook)
(add-hook 'lisp-mode-hook             'paredit-with-rainbow-hook)
(add-hook 'lisp-interaction-mode-hook 'paredit-with-rainbow-hook)
(add-hook 'scheme-mode-hook           'paredit-with-rainbow-hook)
(add-hook 'clojure-mode-hook          'paredit-with-rainbow-hook)

;; autopair mode
(require 'autopair)
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'ruby-mode-hook #'(lambda () (autopair-mode)))

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-prompt-for-directory t)
 '(js-indent-level 2)
 '(scss-compile-at-save nil)
 '(show-trailing-whitespace t)
 '(zencoding-indentation 2))

;; java plugins
(require 'javadoc-lookup)

;; filecache helper function, uses file-cache-add-directory-recursively
;; to add search path
(require 'filecache)
(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))
(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

;; Although "s-p" works on both Mac and Linux, we still put
;; "C-c f" here in case it conflicts with some app
(global-set-key (kbd "C-c f") 'file-cache-ido-find-file)
(global-set-key (kbd "M-p") 'file-cache-ido-find-file)

;; zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)

;; lusty explorer
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(defun my-lusty-hook ()
  (define-key lusty-mode-map (kbd "RET") 'lusty-select-match)
  (define-key lusty-mode-map "\C-o" 'lusty-open-this))
(add-hook 'lusty-setup-hook 'my-lusty-hook)

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; coffee mode
(require 'coffee-mode)

;; haml mode
(require 'haml-mode)

;; sass mode
(require 'sass-mode)

;; scss mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; slim mode
(require 'slim-mode)

;; less css mode
(require 'less-css-mode)

;; handlebars mode
(require 'handlebars-mode)
(add-hook 'handlebars-mode-hook 'zencoding-mode)

;; mustache mode
(require 'mustache-mode)

;; TODO: currently, the following setup only works with mac
(when (equal system-type 'darwin)

  ;; ;; erlang
  ;; (setq erlang-root-dir "/usr/local/Cellar/erlang/R15B02")
  ;; (setq erlang-man-root-dir "/usr/local/Cellar/erlang/R15B02/share/man")

  ;; ;; (add-to-list 'exec-path "/usr/local/Cellar/erlang/R15B02/bin")
  ;; (add-to-list 'load-path "/usr/local/Cellar/erlang/R15B02/lib/erlang/lib/tools-2.6.8/emacs/")
  ;; (require 'erlang-start)

  ;; (add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

  ;; ;; distel
  ;; (add-to-list 'load-path "/usr/local/share/distel/elisp")
  ;; (require 'distel)
  ;; (distel-setup)

  ;; auctex
  ;; On mac, we install auctex via homebrew
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
  (require 'tex-site)

  ;; ;; w3m
  ;; (add-to-list 'load-path "/Applications/Emacs.app/Contents/share/emacs/site-lisp/w3m")
  ;; (require 'w3m-load)
  ;; (setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
  ;;                                     ("api" . w3m-browse-url)
  ;;                                     ("." . browse-url-default-macosx-browser)))
  ;; go
  (add-to-list 'load-path "/usr/local/go/misc/emacs")
  (require 'go-mode-load)

  ;; pbcopy
  ;; (require 'pbcopy)
  ;; (turn-on-pbcopy)
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
