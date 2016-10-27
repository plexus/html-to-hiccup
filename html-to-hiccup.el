;;; html-to-hiccup.el --- Convert HTML to Hiccup syntax

;; Copyright (C) 2016 Arne Brasseur, Jack Rusher

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; URL: https://github.com/plexus/html-to-hiccup
;; Version: 1.0
;; Created: 27 October 2016
;; Keywords: HTML Hiccup Clojure
;; Package-Requires: ((emacs-25.1) (dash "2.13.0") (s "1.10.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Attribution :

;; The first version was written by Arne Brasseur, then vastly cleaned up by
;; Jack Rusher, then Arne gave it some finishing touches.

;;; Commentary:

;; There is a single interactive command, html-to-hiccup-convert-region, which
;; parses the current region as HTML, and replaces it with the equivalent
;; Hiccup. This is especially useful when doing Clojure development and copying
;; HTML snippets from the web.

;; This package does not contain any keybindings, bind it to whatever you see
;; fit.

;; Goes well with `cider-format-edn-region', or with fresh lemonade.

;;; Code:

(require 's)
(require 'dash)

(defun html-to-hiccup/sexp-to-hiccup-tag (elem)
  "Generate Hiccup for a HTML element tag + id/class shorthands."
  (let ((attrs (cadr elem)))
    (concat ":" (symbol-name (car elem))
            (when-let ((id (cdr (assoc 'id attrs))))
              (concat "#" id))
            (when-let ((class (cdr (assoc 'class attrs))))
              (concat "." (s-replace " " "." (s-trim class)))))))

(defun html-to-hiccup/sexp-to-hiccup-attrs (attrs)
  "Generate a Hiccup attributes map."
  (if-let ((attrs (--map (concat ":" (symbol-name (car it))
                                 " " (format "%S" (cdr it)))
                         (assq-delete-all 'class
                          (assq-delete-all 'id attrs)))))
      (concat " {" (s-join " " attrs) "}")))

(defun html-to-hiccup/sexp-to-hiccup-children (cs)
  "Recursively render Hiccup children, skipping empty (whitespace) strings."
  (s-join "" (--map (if (stringp it)
                        (when (string-match "[^\s\n]" it) ; contains non-whitespace
                          (format " %S" it))
                      (concat " " (html-to-hiccup/sexp-to-hiccup it)))
                    cs)))

(defun html-to-hiccup/sexp-to-hiccup (html-sexp)
  "Turn a `html-sexp' (as returned by libxml-parse-*) into a Hiccup element."
  (concat "["
          (html-to-hiccup/sexp-to-hiccup-tag html-sexp)
          (html-to-hiccup/sexp-to-hiccup-attrs (cadr html-sexp))
          (html-to-hiccup/sexp-to-hiccup-children (cddr html-sexp))
          "]"))

(defun html-to-hiccup-convert-region ()
  "Convert the current region from HTML to Hiccup."
  (interactive)
  (let ((html-sexp (libxml-parse-html-region (point) (mark))))
    (delete-region (point) (mark))
    (insert (html-to-hiccup/sexp-to-hiccup html-sexp))))

(provide 'html-to-hiccup)

;;; html-to-hiccup.el ends here
