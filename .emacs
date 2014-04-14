;; recursively add load path
(let ((default-directory "~/.emacs.d/lisps/"))
  (normal-top-level-add-to-load-path '("."
                                       "clojure-mode"
                                       "code-imports"
                                       "lua-mode"
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
                                       "markdown-mode"
                                       "rainbow-delimiters"
                                       "multiple-cursors"
                                       "zencoding"
                                       "ag"
                                       "handlebars-mode"
                                       "glsl-mode"
                                       "editorconfig-emacs")))

;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1)

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
(setq-default tab-width 2)

;; set default indent to 2 spaces
(setq standard-indent 2)
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)

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

;; use C-t for selection
(global-set-key (kbd "C-t") 'set-mark-command)

(require 'uniquify)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; windmove
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (global-set-key [mouse-4] '(lambda ()
;;                               (interactive)
;;                               (scroll-down 1)))
;;   (global-set-key [mouse-5] '(lambda ()
;;                               (interactive)
;;                               (scroll-up 1)))
;;   (defun track-mouse (e))
;;   (setq mouse-sel-mode t)
;; )

;; by default, kill the whole line
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key (kbd "C-M-k") 'smart-kill-whole-line)

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisps/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; font setting
(set-default-font "Dejavu Sans Mono for Powerline-14")

;; clojure mode
(require 'clojure-mode)

;; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-o") 'ace-jump-char-mode)

;; ag, also use ag to replace ack
(require 'ag)
(setq ag-highlight-search t)
(defalias 'ack 'ag)

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
  (c-set-style "google")
  (c-set-offset 'inextern-lang 0))
(add-hook 'c-mode-common-hook 'google-c-style-hook)

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

(defun surround (begin end open close)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (when (string= close "")
    (setq close open))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))

;; lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

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

;; electric pair mode is enough for non-lisp code
(electric-pair-mode +1)
(delete-selection-mode +1)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search nil)
 '(coffee-tab-width 2)
 '(custom-safe-themes (quote ("e697d31361bb4a0a2c15db5a18b2ff4b2bd256fbebc29fdc72deb802b505eb64" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(js-indent-level 2)
 '(scss-compile-at-save nil)
 '(show-trailing-whitespace t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

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

;; zenconding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'zencoding-mode)
(add-hook 'css-mode-hook  'zencoding-mode) ;; enable Emmet's css abbreviation.
(add-hook 'handlebars-mode-hook 'zencoding-mode)
(add-hook 'zencoding-mode-hook (lambda () (setq zencoding-indentation 2))) ;indent 2 spaces.

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(set-face-attribute 'web-mode-html-tag-face nil :foreground "gold")

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

;; mote mode
(add-to-list 'auto-mode-alist '("\\.mote\\'" . html-mode))

;; GLSL mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; multiple cursors mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-m") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-m") 'mc/mark-all-like-this)

;; arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; Rails
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.hbs.erb\\'" . handlebars-mode))

(global-auto-revert-mode 1)

(require 'tempo-snippets)
(load "editorconfig")

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
  ;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
  ;; (require 'tex-site)

  ;; ;; w3m
  ;; (add-to-list 'load-path "/Applications/Emacs.app/Contents/share/emacs/site-lisp/w3m")
  ;; (require 'w3m-load)
  ;; (setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
  ;;                                     ("api" . w3m-browse-url)
  ;;                                     ("." . browse-url-default-macosx-browser)))

  ;; pbcopy
  (require 'pbcopy)
  (turn-on-pbcopy)

  ;; magit
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; go
  (add-to-list 'load-path "/usr/local/Cellar/go/1.2.1/libexec/misc/emacs" t)
  (require 'go-mode-load)
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
