;;; consult-cscope.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Michael Chen

;; Author: Michael Chen <blorbx@gmail.com>
;; Created: May 13, 2022
;; Version: 0.1.0
;; Keywords: tools
;; Homepage: https://github.com/blorbx/consult-cscope
;; Package-Requires: ((emacs "27.1") (consult "0.17"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides cscope support for the `consult' package. cscope is run
;; asynchronously similar to `consult-grep' and provides previews for the
;; results. `consult-cscope' provides the following functions which correspond
;; to each of the search types cscope can perform:
;;
;;   consult-cscope-symbol
;;   consult-cscope-definition
;;   consult-cscope-called-by
;;   consult-cscope-calling
;;   consult-cscope-text
;;   consult-cscope-egrep
;;   consult-cscope-file
;;   consult-cscope-including
;;   consult-cscope-assignment
;;
;; These functions add the thing-at-point to `consult's "future history" which
;; can be accessed with a bind to the `next-history-element' function.
;; Alternatively, if the `consult-cscope-find-' functions are given a prefix
;; argument, the thing-at-point is automatically used as initial input.
;;
;; This package also includes basic `embark' support with the
;; `embark-consult-goto-cscope' function. This function is automatically enabled
;; when `embark' is available.

;;; Code:

(require 'consult)

(defgroup consult-cscope nil
  "Options for `consult-cscope'."
  :group 'consult)

;;; Customization

(defcustom consult-cscope-program "cscope"
  "The path or name of cscope executable."
  :type 'string)

(defcustom consult-cscope-database-file "cscope.out"
  "The path or filename of the cscope database file.
Can be an absolute or relative path. If a relative path, looks in
`default-directory', then checks `projectile-project-root' for the database
file."
  :type 'string)

(defcustom consult-cscope-args "-d"
  "Extra command line arguments for cscope.
Don't use -f or -L, these are automatically used."
  :type 'string)

(defcustom consult-cscope-use-initial nil
  "If non-nil, provide thing-at-point as initial input by default and disable
  with prefix argument. If nil, only provide initial input with prefix
  argument.")

;;; Internal variables

(defvar consult--cscope-history nil)

(defvar consult--cscope-database-finder
  #'consult--cscope-find-database-file
  "Function used to find location of cscope database file.")

(defconst consult--cscope-match-regexp
  "\\`\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)"
  "Regexp used to match file, line, and function of cscope output.")

;;; consult-cscope

(defun consult--cscope-format (async builder)
  "Return ASYNC function highlighting cscope match results.
BUILDER is the command argument builder."
  (let ((highlight))
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (plist-get (funcall builder action) :highlight))
        (funcall async action))
       ((consp action)
        (let (result)
          (save-match-data
            (dolist (str action)
              (when (string-match consult--cscope-match-regexp str)
                (let* ((file (match-string 1 str))
                       (func (match-string 2 str))
                       (line (match-string 3 str))
                       (content (match-string 4 str)))
                  (when (> (length content) consult-grep-max-columns)
                    (setq content (substring content 0 consult-grep-max-columns)))
                  (when highlight
                    (funcall highlight content))
                  (setq str (format "%s:%s:%s: %s"
                                    (propertize file 'face 'consult-file
                                                'consult--cscope-file file)
                                    (propertize line 'face 'consult-line-number)
                                    (propertize func 'face 'consult-bookmark)
                                    content))
                  (push str result)))))
          (funcall async (nreverse result))))
       (t (funcall async action))))))

(defun consult--cscope-state ()
  "Cscope preview state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (when (eq action 'exit)
        (funcall open))
      (funcall jump action (consult--cscope-position cand open)))))

(defun consult--cscope-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (if transform
      (substring cand
                 (1+ (length (get-text-property 0 'consult--cscope-file cand))))
    (get-text-property 0 'consult--cscope-file cand)))

(defun consult--cscope-position (cand &optional find-file)
  "Return the csope position marker for CAND.
FIND-FILE is the file open function, defaulting to `find-file'."
  (when cand
    (let* ((file-end (next-single-property-change 0 'face cand))
           (line-end (next-single-property-change (+ 1 file-end) 'face cand))
           (col 0) ; cscope eats indentation so can't determine
           (file (substring-no-properties cand 0 file-end))
           (line (string-to-number (substring-no-properties cand (+ 1 file-end) line-end))))
      (consult--position-marker
       (funcall (or find-file #'find-file) file)
       line col))))

(defun consult--cscope-builder (input numpattern cscope)
  "Build command line given INPUT.
NUMPATTERN and CSCOPE are used to build the cscope command.
CSCOPE should be a list with the form (program args db-file)."
  (pcase-let* ((`(,program ,args ,db-file) cscope)
               (cmd (split-string-and-unquote
                     (format "%s %s -f %s -L%s"
                             program args db-file numpattern)))
               (type 'basic)
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg type
                                      (or (member "-C" cmd)
                                          (member "-C" opts)))))
    (when re
      (list :command
            (append cmd
                    re
                    opts)
            :highlight hl))))

(defun consult--cscope (prompt builder dir initialp thing)
  "Run cscope with database in DIR directory.

PROMPT is the prompt.
BUILDER is the command builder.
DIR is the directory of the cscope database file.
INITIALP is true if using initial input.
THING is the type of symbol used by `thing-at-point' for initial input."
  (let* ((prompt-dir (consult--directory-prompt prompt dir))
         (default-directory (cdr prompt-dir))
         (read-process-output-max (max read-process-output-max (* 1024 1024)))
         (thingatpt (consult--async-split-thingatpt thing))
         (initial (if (and initialp thingatpt) thingatpt
                    (consult--async-split-initial nil))))
    (consult--read
     (consult--async-command builder
       (consult--cscope-format builder)
       :file-handler t) ;; allow tramp
     :prompt (car prompt-dir)
     :lookup #'consult--lookup-member
     :state (consult--cscope-state)
     :initial initial
     :add-history thingatpt
     :require-match t
     :category 'consult-cscope
     :group #'consult--cscope-group
     :history '(:input consult--cscope-history)
     :sort nil)))

(defun consult--cscope-find-database-file (start-dir)
  "Look for cscope database file (cscope.out) starting in START-DIR.

Search is based on variable `consult-cscope-database-file'. If
`consult-cscope-database-file' is an absolute path and the file exists, return
it. Otherwise, look for `consult-cscope-database-file' in START-DIR. If that
doesn't exist, use `projectile-project-root' if available to look in the project
root."
  (pcase consult-cscope-database-file
    ((or
      ;; Absolute path, return if exists
      (and (pred file-name-absolute-p)
           (pred file-exists-p)
           db-path)
      ;; Relative path, expand with start-dir
      (and (app file-name-absolute-p 'nil)
           db-file
           (let db-path (expand-file-name db-file start-dir))
           (guard (file-exists-p db-path)))
      ;; Relative path, check project root
      (and (app file-name-absolute-p 'nil)
           db-file
           (guard (fboundp #'projectile-project-root))
           (let db-path (expand-file-name db-file (projectile-project-root)))
           (guard (file-exists-p db-path))))
     db-path)))

(defun consult--cscope-search (numpattern prompt start-dir initialp thing)
  "Perform a NUMPATTERN type search with cscope.

Start cscope database search in START-DIR. Show PROMPT in minibuffer. INITIALP
controls whether or not initial input should be provided. THING is the type of
symbol used by `thing-at-point' for initial input."
  (let* ((db-file (funcall consult--cscope-database-finder start-dir))
         (db-dir (and db-file (file-name-directory db-file)))
         (program consult-cscope-program) ; Use buffer-local vars
         (args consult-cscope-args)
         (initialp (xor initialp consult-cscope-use-initial))
         (builder (lambda (input)
                    (consult--cscope-builder
                     input numpattern `(,program ,args ,db-file)))))
    (if db-file
        (consult--cscope prompt builder db-dir initialp thing)
      (user-error "Cscope database file `%s' not found"
                  consult-cscope-database-file))))

;;;###autoload
(defun consult-cscope-symbol (arg)
  "Use cscope to find this symbol.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "0" "Find this symbol"
                          default-directory arg 'symbol))

;;;###autoload
(defun consult-cscope-definition (arg)
  "Use cscope to find this global definition.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "1" "Find this global definition"
                          default-directory arg 'symbol))

;;;###autoload
(defun consult-cscope-called-by (arg)
  "Use cscope to find functions called by this function.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "2" "Find functions called by this function"
                          default-directory arg 'symbol))

;;;###autoload
(defun consult-cscope-calling (arg)
  "Use cscope to find functions calling this function.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "3" "Find functions calling this function"
                          default-directory arg 'symbol))

;;;###autoload
(defun consult-cscope-text (arg)
  "Use cscope to find this text string.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "4" "Find this text string"
                          default-directory arg 'word))

;;;###autoload
(defun consult-cscope-egrep (arg)
  "Use cscope to find this egrep pattern.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "6" "Find this egrep pattern"
                          default-directory arg 'word))

;;;###autoload
(defun consult-cscope-file (arg)
  "Use cscope to find this file.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "7" "Find this file"
                          default-directory arg 'filename))

;;;###autoload
(defun consult-cscope-including (arg)
  "Use cscope to find files including this file.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "8" "Find files including this file"
                          default-directory arg 'filename))

;;;###autoload
(defun consult-cscope-assignment (arg)
  "Use cscope to find assignments to this symbol.

The symbol at point as initial input if prefix ARG provided.  The symbol at
point is also added to consult's future history."
  (interactive "P")
  (consult--cscope-search "9" "Find assignments to this symbol"
                          default-directory arg 'symbol))

;;; Embark support

(with-eval-after-load 'embark
  (defun embark-consult-goto-cscope (location)
    "Go to LOCATION, which should be a string with a cscope match."
    (let ((default-directory (with-selected-window (previous-window)
                               default-directory)))
      (consult--jump (consult--cscope-position location))
      (pulse-momentary-highlight-one-line (point))))

  (setf (alist-get 'consult-cscope embark-default-action-overrides)
        #'embark-consult-goto-cscope))

(provide 'consult-cscope)
;;; consult-cscope.el ends here
