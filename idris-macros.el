;;; idris-macros.el --- additional tools for editing idris -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: idris
;; Version: 0.0.2
;; URL: https://github.com/conjunctive/idris-macros
;; Package-Requires: ((emacs "26") cl-lib idris-mode s)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'idris-mode)
(require 'simple)
(require 'subr-x)
(require 's)

(defvar idris-fun-decl-rgx
  "\\([[:alnum:]]*\\)[[:space:]]+\\(:\\)[[:space:]]+\\(.?*\\)\\(\n\\)\\([[:alnum:]]*\\)[[:space:]]*\\(.?*\\)[[:space:]]+\\(=\\)[[:space:]]+\\(.?*\\)$"
  "When searching for a function declaration:
From the first line capture the identifier, colon, and type.
Capture the newline in-between the two lines.
From the second line capture the identifier, equals sign, and value.")

(defun idris-repl-send-string (str)
  "Send string to Idris REPL buffer, then evaluate."
  (with-current-buffer (idris-repl-buffer)
    (let ((inhibit-read-only t))
      (goto-char idris-input-start)
      (idris-repl-delete-current-input)
      (insert str)
      (idris-repl-return))))

(defun idris-preserve-fn-args (str)
  "Preserve function argument patttern matching.
Where STR is all of the arguments separated by spaces.
Returns an argument declaration format suitable for anonymous functions.

'xs   (Just ys)' => 'xs, (Just ys)'"
  (unless (string-blank-p str)
    (save-excursion
      (save-match-data
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (let ((acc (list)))
            (while (re-search-forward "\\(\(.?*\)\\|[[:word:]]+\\)" nil t)
              (push (match-string 0) acc))
            (cl-reduce (lambda (v acc) (concat acc ", " v))
                       acc
                       :from-end t)))))))

(defun idris-repl-parse-fn-decl (str)
  "Attempts to convert the function declaration STR into a top-level :let command.
This lets us send function declarations, where the body is a single line, to the REPL.

f : Int -> Int -> Int
f x y = x + y
=>
:let f = the (Int -> Int -> Int) (\x, y => x + y)"
  (with-temp-buffer
    (idris-mode)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward idris-fun-decl-rgx nil t)
        (if-let ((type-ident (match-string 1)))
            (progn
              ;; "f : ..." => ":let f = ..."
              (replace-match (concat ":let " type-ident) nil nil nil 1)
              (replace-match "=" nil nil nil 2)
              (if-let ((type-sig (match-string 3)))
                  (progn
                    ;; ":let f = Int -> Int" => ":let f = the (Int -> Int) ..."
                    (replace-match (concat "the (" type-sig ") ") nil nil nil 3)
                    (replace-match "" nil nil nil 4)
                    (if (match-string 5)
                        (progn
                          (replace-match "" nil nil nil 5)
                          (let ((fn-args (idris-preserve-fn-args (match-string 6))))
                            (replace-match "" nil nil nil 6)
                            (replace-match "" nil nil nil 7)
                            (if-let ((fn-body (match-string 8)))
                                ;; ":let f = the (Int -> Int)
                                ;;  f x = x + 1"
                                ;;  =>
                                ;; ":let f = the (Int -> Int) (\x => x + 1)"
                                (progn
                                  (replace-match (concat "(" (if fn-args
                                                                 (concat "\\\\" fn-args "\s=> " fn-body)
                                                               fn-body)
                                                         ") ")
                                                 nil nil nil 8)
                                  (while (re-search-forward "\n" nil t)
                                    (replace-match " "))
                                  (goto-char (point-min))
                                  (while (re-search-forward "\s[\s]+" nil t)
                                    (replace-match "\s"))
                                  (buffer-string))
                              (error "Could not find function body"))))
                      (error "Could not find identifier from function declaration")))
                (error "Could not find type signature")))
          (error "Could not find identifier from type signature"))
      (error "Could not find function declaration"))))

;;;###autoload
(defun idris-repl-eval-region ()
  "Evaluate the region as Idris code.
Single expression evaluation works for comments.
Attempt to convert function declarations to :let commands."
  (interactive)
  (when (and (region-active-p) (not (region-noncontiguous-p)))
    (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
      (idris-repl-send-string (if (string-match-p idris-fun-decl-rgx str)
                                  (idris-repl-parse-fn-decl str)
                                (thread-last str
                                  (replace-regexp-in-string "^-- " "")
                                  (s-trim-left)
                                  (replace-regexp-in-string "\n" " ")))))))

(defun idris-switch-to-repl ()
  "Switch to the Idris REPL, initialize if not ready."
  (interactive)
  (condition-case nil (idris-pop-to-repl)
    (error (idris-repl))))

(provide 'idris-macros)

;;; idris-macros.el ends here
