;;; arbos.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Rodriguez
;;
;; Original-author: Victor Rodriguez <https://github.com/vrodriguez>
;; Maintainer: Martin Petkovski <https://github.com/mpetco>
;; Created: April 26, 2021
;; Forked-since: August 12, 2025
;; Version: 0.0.1
;; Keywords: note-taking org-mode
;; Homepage: https://github.com/mpetco/arbos
;; Package-Requires: ((emacs "24.3"))
;;

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package is a fork of dendroam by Victor Rodriguez. The intent is to stick closely to a hierarchical note taking philosophy while decoupling away from roam.
;; Arbos means tree in latin.
;;
;;; Code:



(provide 'arbos)

(defvar arbos-directory)

(defvar arbos-capture-templates
  '(("t" "Time note" entry
     "* %?"
     :if-new (file+head "${current-file}.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n"))
    ("s" "Scratch note" entry
     "* %?"
     :if-new (file+head "scratch.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n")))

  "Some utils templates for different type of notes such us time notes
or sratch notes")

;;Node custom getters
(cl-defmethod org-roam-node-current-file (node)
  "Gets node file-name-base by file name"
  (file-name-base (org-roam-node-file node)))

(cl-defmethod org-roam-node-hierarchy-title (node)
  "Gets node title excluding the hierarchy and capitalize it"
  (capitalize
   (car
    (last
     (split-string
      (org-roam-node-title node)
      "\\.")))))

(defun arbos-format-hierarchy (file)
  "Formats node's path, to get the hierarchy whithout the title
where title will be the last child of the hierarchy:
from the filename this.is.a.hierarchy.note-title.org
returns this.is.a.hierarchy"
  (let* ((base-name (file-name-base file))
         (hierarchy-no-title (file-name-base base-name)))
    hierarchy-no-title))

(cl-defmethod org-roam-node-hierarchy (node)
  "Gets node hierarchy by file name"
  (funcall 'arbos-format-hierarchy (org-roam-node-file node)))

(cl-defmethod org-roam-node-current-file (node)
  (file-name-base (buffer-file-name)))

;; Refactor functions

(defun arbos-fetch-same-hierarchy-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY totally or parcially"
  (let ((files
         (mapcar #'car (org-roam-db-query [:select [file]
                                           :from nodes
                                           :where (like file $r1)]
                                          (concat "%" hierarchy "%")))))
    files))

(defun arbos-refactor-hierarchy (&optional current)
  "Prompts the user to change the hierarchy of the current file
node and updates its hierarchy and the hierarchy of all the nodes
that have it if CURRENT is t the list of updated files is just
the current file"
  (interactive)
  (let*
      ((initial-file
        (file-name-nondirectory (buffer-file-name)))
       (initial-slug
        (file-name-base initial-file))
       (new-slug (file-name-base
                  (read-string "Refactor: " initial-slug)))
       (initial-slug-no-title
        (file-name-base initial-slug))
       (files-to-upd (if current
                         `(,initial-file)
                       (arbos-fetch-same-hierarchy-files
                        initial-slug-no-title))))

    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))

(defun arbos-refactor-file ()
  (interactive)
  (let* ((initial-file (buffer-file-name))
         (initial-slug (file-name-base initial-file))
         (new-slug (read-string "Refactor: " initial-slug))
         (new-file (concat
                    (expand-file-name new-slug org-roam-directory)
                    ".org")))
    (rename-file initial-file new-file)
    (kill-current-buffer)
    (find-file new-file)))

;; Useful notes functions
(defun arbos-insert-time-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `arbos-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates arbos-capture-templates
                     :keys "t"
                     :props (list :default-time (current-time))))

(defun arbos-insert-scratch-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates arbos-capture-templates
                     :keys "s"
                     :props (list :default-time (current-time))))

;; Org roam overrides to allow these features
(eval-after-load "org-roam"
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Override. Generates a dendron-like slug from *title*
this expects an input like: lang.elisp.what is nil
and will create a file wih the name: lang.elisp.what-is-nil"
  (let ((title (org-roam-node-title node))
        (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
               (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                      ("__*" . "_")                   ;; remove sequential underscores
                      ("^_" . "")                     ;; remove starting underscore
                      ("_$" . "")))                   ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug))))))
;;; arbos.el ends here
