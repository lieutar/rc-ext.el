;;; rc-ext.el --- 

;; Copyright (C) 2010  

;; Author:  <lieutar at 1dk.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'url)
(require 'cl)




;;;
;;; options
;;;
(defvar rc-ext:with-rc-boot (boundp 'rc-emacsen))
(unless rc-ext:with-rc-boot
  (defvar rc-emacsen   "emacs")
  (defvar rc-boot-current-loading-file nil))
(defvar rc-site-lisp (expand-file-name
                      (concat "~/.emacs.d/rc/site-lisp." rc-emacsen)))

(unless (file-exists-p rc-site-lisp)
  (make-directory rc-site-lisp))

(unless (member rc-site-lisp load-path)
  (setq load-path (cons rc-site-lisp load-path)))

(defvar rc-ext-classes-alist      ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rc-get
;;;

(labels
    ((http-get
      (url)
      (let ((buffer (url-retrieve-synchronously url)))
        (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let ((header-max  (point))
                (headers     ())
                status
                http-version
                status-code
                status-message)
            (goto-char (point-min))
            (re-search-forward
             "^HTTP/\\([0-9\\.]+\\)\\s +\\([0-9]+\\)\\s +\\([^\n\r]+\\)"
             nil t nil)
            (setq status         (match-string 0))
            (setq http-version   (match-string 1))
            (setq status-code    (string-to-number (match-string 2)))
            (setq status-message (match-string 3))
           (let ((field   nil)
                 (content ""))
             (while (< (point) header-max)
               (next-line)
               (cond ((re-search-forward
                       "^\\([A-Za-z0-9\\-]+\\):\\s +\\([^\n\r]+\\)"
                       header-max t nil)
                      (when field
                        (setq headers (cons (cons field content) headers)))
                      (setq field   (match-string 1))
                      (setq content (match-string 2)))
                     ((re-search-forward
                       "^\\s +\\([^\r\n]+\\)" 
                       header-max t nil)
                      (setq content (concat content (match-string 1))))
                     (t
                      ()))
               )
             (when field
               (setq headers (cons (cons field content) headers))))
           (goto-char header-max)
           (next-line)
           (beginning-of-line)
           (delete-region (point-min) (point))
           `((url            . ,url)
             (status         . ,status)
             (http-version   . ,http-version)
             (status-code    . ,status-code)
             (status-message . ,status-message)
             (content        . ,buffer)
             ,@headers)))))

     (hexchar2num
      (chr)
      (if (< chr ?A)
          (- chr ?0)
        (+ 10 (- chr (if (< chr ?a) ?A ?a)))))

     (uri-decode
      (str)
      (let ((pos 0))
        (while (string-match
                "\\(%[0-9A-Fa-f][0-9A-Fa-f]\\|\\+\\)" str pos)
          (let* ((mstr (match-string    1 str))
                 (head (substring str 0 (match-beginning 1)))
                 (tail (substring str (match-end 1))))
            (setq pos (1+ (match-beginning 1)))
            (setq str (concat head
                              (if (equal mstr "+") " "
                                (string
                                 (+ (* 16 (hexchar2num
                                           (aref mstr 1)))
                                    (hexchar2num (aref mstr 2)))))
                              tail)))))
      str)

     (with-url
      (url callback &optional accept-statuses)
      (let* ((res  (http-get url))
             (code (cdr (assoc 'status-code res)))
             (asp  (cond ((null  accept-statuses)
                          (lambda (n) (and (> n 199) (< n 300))))
                         ((listp accept-statuses)
                          (lambda (n)
                            (and (member n accept-statuses) t)))
                         ((functionp accept-statuses)
                          accept-statuses)
                         (t (error
                             "wrong type argument: function or list"))
                         )))
        (if (apply asp (list code))
            (progn
              (apply callback (list (cdr (assoc 'content res)) res)))
          (throw 'illegal-http-code res)))))

  (defun rc-get:response-filename (res)
    (let ((url (replace-regexp-in-string
                "\\?.*$" ""
                (cdr (assoc 'url res)))))
      (when (and url
                 (string-match "\\([^/]+\\)$" url))
        (uri-decode (match-string 1 url)))))


  (defun rc-get (url &rest opts)
    (let* ((opts        (if (= 1 (length opts))
                            (list :compile (car opts))
                          opts))
           (not-compile (and (member :compile opts)
                             (not (plist-get :compile opts))))
           (opt-filename    (plist-get :filename opts)))
    (with-url
     url
     `(lambda (buf res)
        (save-excursion
          (let ((filename
                 (expand-file-name (concat rc-site-lisp "/"
                                           (or ,opt-filename
                                               (rc-get:response-filename res))))))
            (set-buffer buf)
            (setq buffer-file-name filename)
            (replace-string "" "")
            (save-buffer)
            (kill-buffer buf)
            ,(unless not-compile '(byte-compile-file filename))
            filename)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; debugging utilities
;;;

(defun rc-ext:debug:backtrace1 (cont debugger-args skip)
  (format "%S\nbacktrace:%s"
          debugger-args
          (let ((drop  t)
                (depth 0)
                (trace t)
                (skip (or skip 0))
                (all  ""))
            (while trace
              (unless (eq trace t)
                (if drop
                    (if (eq (cadr trace) 'rc-ext:debug:backtrace)
                        (setq drop nil))
                  (if (> 1 skip)
                      (setq all
                            (concat
                             all
                             "\n"
                             (format "\t%S" (cdr trace))))
                    (setq skip (1- skip)))))
              (setq trace (backtrace-frame depth))
              (setq depth (1+ depth))
              (when (and (eq    (cadr  trace) 'catch)
                         (equal (caddr trace) (list 'quote cont)))
                (setq trace nil)))
            all)))

(defun rc-ext:debug:backtrace (cont debugger-args &optional skip)
  ""
  (throw cont (rc-ext:debug:backtrace1 cont debugger-args skip)))



(defmacro rc-ext:with-backtrace (&rest form)
  (let ((tag 'RC-EXT::ERROR!!))
    `(let* ((debugger (lambda (&rest args)
                        (rc-ext:debug:backtrace ',tag
                                                args
                                                1)))
            (real-result   nil)
            (result (catch ',tag
                      (setq real-result
                            (progn ,@form))
                      :done)))
       (if (eq result :done)
           real-result
         (error result)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rc-ext:file-with-face (file)
  (if file
      (let ((km (make-sparse-keymap))
            (str (copy-sequence file)))
        (define-key km (kbd "<RET>") `(lambda ()
                                        (interactive)
                                        (find-file ,file)))
        (propertize str
                    `keymap km
                    'face '((:underline t))))
    ""))

(lexical-let ((error-buffer nil))

  (labels ((with-error-buffer
            (cb)
            (progn
              (unless (buffer-live-p error-buffer)
                (setq error-buffer (get-buffer-create "*rc-ext-errors*")))
              (save-excursion
                (set-buffer error-buffer)
                 (funcall cb)))))

    (defun rc-ext:log (form &rest vals)
      (with-error-buffer
       (lambda () (insert (apply 'format form vals) "\n"))))

    (defun rc-ext:error-message (form &rest vals)
      (apply 'message form vals)
      (with-error-buffer
       (lambda () (insert (apply 'format form vals) "\n"))))

    (defun rc-ext:error (form &rest vals)
      (apply 'rc-ext:error-message form vals)
      (error (apply 'format form vals)))
    ))

;;(rc-ext:error-message "xxx")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; module system
;;;
(defvar rc-ext:$mods-alist    ())
(defvar rc-ext:$provided      ())
(defvar rc-ext:$waiting-plist ())

(defun rc-ext:provide (feature body)
  (if (member feature rc-ext:$provided)
      t
    (let ((R (funcall body)))

      (rc-ext:log "provide ... execute body %s -> %S" feature R)

      (setq rc-ext:$provided (cons feature rc-ext:$provided))

      (dolist (mod (plist-get rc-ext:$waiting-plist feature))

        (let ((n-depends (1- (caddr mod))))
          (setcdr (cdr mod) (list n-depends))

          (rc-ext:log "    -> %S" mod)

          (when (zerop n-depends)
            (rc-ext:provide (car mod) (cadr mod)))))


      R)))

(defun rc-ext:defconfig (name depends body file)
  (setq rc-ext:$mods-alist (cons (list name depends body file)
                                 rc-ext:$mods-alist)))

(defun rc-ext:run (name)
  (unless (member name rc-ext:$provided)
    (let ((config  (assq name rc-ext:$mods-alist)))
      (if config
          (let ((depends (cadr config))
                (body    (caddr config))
                (deplen  0))
            
            (dolist (dep depends)
              (unless (member dep rc-ext:$provided)
                (setq deplen (1+ deplen))))
            
            (if (zerop deplen)
                (rc-ext:provide name body)
              
              (let ((modinfo (list name body deplen)))
                (dolist (dep depends)
                  (let ((slot (plist-get rc-ext:$waiting-plist dep)))
                    (if slot
                        (plist-set rc-ext:$waiting-plist
                                   dep
                                   (cons modinfo slot))
                      (setq rc-ext:$waiting-plist
                            (cons dep (cons (list modinfo)
                                            rc-ext:$waiting-plist)))))))))
        (rc-ext:error "unknown config %s" name)))))

(lexical-let ((last-error nil))
  (defun rc-ext:lazy-run (name)
    (rc-ext:log "lazy-run: %s" name)
    (if (member name rc-ext:$provided) t
      (let ((config  (assq name rc-ext:$mods-alist)))
        (when config
          (let ((depends (cadr config))
                (body    (caddr config))
                (file    (cadddr config)))

            (condition-case err

                (progn
                  (dolist (dep depends)
                    (rc-ext:log "load depends: %s" dep)
                    (unless (rc-ext:lazy-run dep)
                      (error (or last-error
                                 "???"))))
                  (rc-ext:log "lazy provide %s" name)
                  (rc-ext:provide name body))

              (error (setq last-error err)
                     (rc-ext:error-message "error lazy-run: %s %s %s"
                                           name
                                           file
                                           err)
                     nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions for main api
;;;

(defun rc-ext:execute-body (name path load get init file  retry)
  (rc-ext:log "execute-body %s" name)
  (let ((rc-site-lisp
         (expand-file-name
          (if path
              (if (string-match "^\\(\\([a-zA-Z]:\\)?[/\\\\]\\|~\\)" path)
                  path
                (concat rc-site-lisp "/" path))
            rc-site-lisp))))
    (unless (member rc-site-lisp load-path)
      (setq load-path (cons rc-site-lisp load-path)))
    (unless (file-exists-p rc-site-lisp)
      (make-directory rc-site-lisp))
    (when (condition-case e
              (rc-ext:with-backtrace
                (rc-ext:log "load %s: %s" name (rc-ext:file-with-face file))
                (apply load ())
                t)
            (error 
             (rc-ext:error-message
              "loading-error: %s : %S %S"
              (rc-ext:file-with-face file)
              e
              load)
             (if retry
                 (progn
                   (apply get ())
                   (rc-ext:execute-body name path load get init file nil))
               nil)))
      (condition-case e
          (rc-ext:with-backtrace
            (rc-ext:log "init %s: %s" name (rc-ext:file-with-face file))
            (apply init ())
            (rc-ext:log "init ... done %s: %s" name (rc-ext:file-with-face file))
            t)
        (error
         (rc-ext:error-message
          "init-error: %s : %S"
          (rc-ext:file-with-face file)
          e)
         nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; main api
;;;

(defvar rc-ext-current-class        nil)
(lexical-let ((symcounter 0))

  (defun rc-ext (&rest args)
    (let* ((name     (plist-get args     :name))
           (cfg-name (or name (intern (format "config-%d"
                                              (setq symcounter
                                                    (1+ symcounter))))))

           (requires (plist-get args :requires))

           (file     (or (plist-get args :file)
                         rc-boot-current-loading-file))

           (funcs    (plist-get args :autoload))

           (class    (plist-get args :class   ))

           (path     (plist-get args :path    ))

           (load     (let ((load (if (member :load args)
                                     (plist-get args :load)
                                   name)))
                       (if (and load (symbolp load))
                           `(lambda () (require ',load))
                         (or load (lambda ())))))

           (get      (let ((get (plist-get args :get     )))
                       (if (stringp get)
                           `(lambda () (rc-get ,get))
                         (or get  (lambda ())))))

           (init     (or (plist-get args :init    )
                         (lambda ())))

           (preload  (or (plist-get args :preload )
                         (lambda ())))

           (exec      (and
                       (apply (or (plist-get args :cond)
                                  (lambda () t))
                              ())
                       (if class
                           (let ((classdef (assq class rc-ext-classes-alist)))
                             (and classdef
                                  (eq name (cdr classdef))))
                         t))))

      (when exec
        (apply preload ())
        (rc-ext:defconfig
         cfg-name
         requires
         `(lambda ()
            (let ((rc-ext-current-class ',class))
              (rc-ext:execute-body ',cfg-name ,path ,load ,get ,init ,file t)))
         file)

          (if funcs
              (let ((funcs (mapcar (lambda (slot)
                                     (or (and (consp slot) slot)
                                         (cons slot (format "%s" slot))))
                                   (if (listp funcs) funcs  (list funcs)))))
                (dolist (slot funcs)
                  (let ((sym (car slot))
                        (desc (cdr slot)))
                    (fset sym
                          `(lambda (&rest args)
                             ,desc
                             (interactive)
                             (dolist (alsym ',(mapcar 'car funcs))
                             (fset alsym nil))
                             (if (rc-ext:lazy-run ',cfg-name)
                                 (if (called-interactively-p)
                                     (call-interactively ',sym)
                                   (apply ',sym args))
                               (rc-ext:error-message
                                "initialize failed: %s %s"
                                ',cfg-name
                                (rc-ext:file-with-face ,file))))))))

            (rc-ext:run cfg-name)))))

  );; end of lexical-let

  
  
(provide 'rc-ext)
;;; rc-ext.el ends here
