;;; html-to-hiccup.el --- Convert HTML to Hiccup syntax   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Arne Brasseur, Jack Rusher

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; URL: https://github.com/plexus/html-to-hiccup
;; Version: 1.0
;; Created: 27 October 2016
;; Keywords: HTML Hiccup Clojure convenience tools
;; Package-Requires: ((emacs "25.1") (s "1.10.0"))

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

;; There is a single interactive command, `html-to-hiccup-convert-region',
;; which parses the current region as HTML, and replaces it with the
;; equivalent Hiccup.  This is especially useful when doing Clojure
;; development and copying HTML snippets from the web.

;; This package does not contain any keybindings, bind it to whatever you see
;; fit.

;; Goes well with `cider-format-edn-region', or with fresh lemonade.

;;; Code:

(require 's)
(require 'seq)
(require 'subr-x)

(defgroup html-to-hiccup nil
  "Convert HTML to Hiccup syntax."
  :prefix "html-to-hiccup-"
  :group 'tools)

(defcustom html-to-hiccup-use-shorthand-p t
  "If non-nil, use shorthand notation for class attributes when possible."
  :type 'boolean
  :safe #'booleanp)

(setq html-to-hiccup--case-sensitive-attrs
      (mapcar
       (lambda (tag)
         (cons (downcase tag) tag))
       '("externalResourcesRequired" "focusHighlight" "requiredExtensions" "requiredFeatures" "requiredFonts"
         "requiredFormats" "systemLanguage" "glyphRef" "attributeName" "attributeType"
         "calcMode" "keySplines" "keyTimes" "repeatCount" "repeatDur"
         "keyPoints" "initialVisibility" "preserveAspectRatio" "syncBehavior" "syncMaster"
         "syncTolerance" "pathLength" "clipPathUnits" "edgeMode" "kernelMatrix"
         "kernelUnitLength" "preserveAlpha" "targetX" "targetY" "diffuseConstant"
         "surfaceScale" "xChannelSelector" "yChannelSelector" "stdDeviation" "tableValues"
         "specularConstant" "specularExponent" "limitingConeAngle" "pointsAtX" "pointsAtY"
         "pointsAtZ" "baseFrequency" "numOctaves" "stitchTiles" "filterRes"
         "filterUnits" "primitiveUnits" "gradientTransform" "gradientUnits" "spreadMethod"
         "defaultAction" "markerHeight" "markerUnits" "markerWidth" "refX"
         "refY" "viewBox" "maskContentUnits" "maskUnits" "patternContentUnits"
         "patternTransform" "patternUnits" "mediaCharacterEncoding" "mediaContentEncodings" "mediaSize"
         "mediaTime" "baseProfile" "contentScriptType" "contentStyleType" "playbackOrder"
         "snapshotTime" "syncBehaviorDefault" "syncToleranceDefault" "timelineBegin" "zoomAndPan"
         "lengthAdjust" "textLength" "startOffset" "transformBehavior" "viewTarget")))

(setq html-to-hiccup--case-sensitive-tags
      (mapcar
       (lambda (tag)
         (cons (downcase tag) tag))
       '("altGlyph" "altGlyphDef" "altGlyphItem" "animateColor" "animateMotion"
         "animateTransform" "clipPath" "feBlend" "feColorMatrix" "feComponentTransfer"
         "feComposite" "feConvolveMatrix" "feDiffuseLighting" "feDisplacementMap" "feDistantLight"
         "feDropShadow" "feFlood" "feFuncA" "feFuncB" "feFuncG"
         "feFuncR" "feGaussianBlur" "feImage" "feMerge" "feMergeNode"
         "feMorphology" "feOffset" "fePointLight" "feSpecularLighting" "feSpotLight"
         "feTile" "feTurbulence" "foreignObject" "glyphRef" "linearGradient"
         "radialGradient" "solidColor" "textArea" "textPath")))

(defun html-to-hiccup--sexp-to-hiccup-tag (elem tag-class?)
  "Generate Hiccup for the HTML ELEM tag + id + (if TAG-CLASS?)
class shorthands."
  (let* ((attrs (cadr elem))
         (tag (symbol-name (car elem)))
         (cased-tag (cdr
                     (assoc tag html-to-hiccup--case-sensitive-tags))))
    (concat ":" (or cased-tag tag)
            (when-let ((id (cdr (assoc 'id attrs))))
              (concat "#" id))
            (when tag-class?
              (when-let ((class (cdr (assoc 'class attrs))))
                (concat "." (s-replace " " "." (s-trim class))))))))

(defun html-to-hiccup--sexp-to-hiccup-attrs (attrs attrs-remove-class?)
  "Generate a Hiccup ATTRS map.
The class attribute is removed when ATTRS-REMOVE-CLASS? is non-nil."
  (if-let ((attrs (mapcar
                   (lambda (it)
                     (let* ((attr (symbol-name (car it)))
                            (cased-attr (cdr
                                         (assoc attr html-to-hiccup--case-sensitive-attrs))))
                       (format ":%s %S" (or cased-attr attr) (cdr it))))
                   (assq-delete-all 'id
                                    (if attrs-remove-class?
                                        (assq-delete-all 'class attrs)
                                      attrs)))))
      (concat " {" (s-join " " attrs) "}")))

(defun html-to-hiccup--sexp-to-hiccup-children (cs)
  "Recursively render Hiccup children CS, skipping empty (whitespace) strings."
  (s-join "" (mapcar (lambda (it)
                       (if (stringp it)
                           (when (string-match "[^\s\n]" it) ; contains non-whitespace
                             (format " %S" it))
                         (concat " " (html-to-hiccup--sexp-to-hiccup it))))
                     cs)))

(defun html-to-hiccup--sexp-to-hiccup (html-sexp)
  "Turn a HTML-SEXP (as returned by libxml-parse-*) into a Hiccup element."
  (let* ((attrs (cadr html-sexp))
         (class (cdr (assoc 'class attrs)))
         (tag-class-shorthand? (and html-to-hiccup-use-shorthand-p
                                    ;; not all class chars are valid for shorthand syntax
                                    (when class (not (s-contains? "/" class))))))
    (concat "["
            (html-to-hiccup--sexp-to-hiccup-tag html-sexp tag-class-shorthand?)
            (html-to-hiccup--sexp-to-hiccup-attrs attrs tag-class-shorthand?)
            (html-to-hiccup--sexp-to-hiccup-children (cddr html-sexp))
            "]")))

;;;###autoload
(defun html-to-hiccup-convert-region (start end &optional bodytags)
  "Convert the region between START and END from HTML to Hiccup.
If BODYTAGS is non-nil, skip the first element returned from the HTML parser."
  (interactive "r")
  (save-restriction
    (let* ((html-str (buffer-substring start end))
           (bodytags (or bodytags
                         (not
                          (string-match-p "<html.*\\(\n.*\\)*<body" html-str)))))
      (narrow-to-region start end)
      (let ((html-sexp (if bodytags
                           (nth 2 (nth 2 (libxml-parse-html-region (point-min) (point-max))))
                         (libxml-parse-html-region (point-min) (point-max)))))
        (delete-region (point-min) (point-max))
        (when (not (stringp html-sexp))
          (insert (html-to-hiccup--sexp-to-hiccup html-sexp)))))))

;;;###autoload
(defun html-to-hiccup-yank (&optional arg)
  "Like `yank' but insert killed HTML as Hiccup.
ARGs are the same as in the `yank' command.

Code is copied from the `yank' function in simple.el."
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark) (current-kill 0)
  (let ((start (point)))
    (insert (current-kill (cond
                           ((listp arg) 0)
                           ((eq arg '-) -2)
                           (t (1- arg)))))
    (html-to-hiccup-convert-region start (point)))

  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (if (eq this-command t)
      (setq this-command 'yank))
  nil)

(provide 'html-to-hiccup)

;;; html-to-hiccup.el ends here
