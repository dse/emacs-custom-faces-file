;;; custom-faces-file.el --- Makes customize save face customizations into a
;;; different file.
;;
;; Filename: custom-faces-file.el
;; Description: Makes customize save face customizations into a different file.
;; Author: Darren Embry
;; Copyright (c) 2008 Darren Embry
;; URL: http://webonastick.com/emacs-lisp/custom-faces-file.el
;; Keywords: customize, faces
;; Compatibility: GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;; GPL 2 is available here:
;;   http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; custom-faces-file: Makes customize save face customizations into a
;; different file whose location is specified by the custom-save-faces
;; variable.
;;
;; Why I wrote this:
;; 
;; - I want my variable customizations in one file.
;; 
;; - I want a separate face customization file for each different kind of
;;   display: terminal, emacsw32, x, etc.
;; 
;; - I find the interface that Customize uses to allow you to set different
;;   attributes for different kinds of displays horrid.
;;
;; - NOTE: customize allows you to specify custom faces for different
;;   display types.  Unfortunately it doesn't work for customizing the
;;   default face.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOW TO USE
;;
;; Just put this file in your Emacs library directory and add this line to
;; your ~/.emacs:
;;
;;   (require 'custom-faces-file)
;;
;; Next, you want to set the custom-faces-file variable to something, then
;; load the file it points to.
;;
;; A simple example:
;;   (setq custom-faces-file "~/.emacs-custom-faces-%s.el")
;;   (load (custom-faces-file))
;;
;; A more complex example:
;;   (cond ((eq window-system 'x)
;;          (setq custom-faces-file "~/.emacs-custom-faces-x.el"))
;;         ((eq window-system nil)
;;          (setq custom-faces-file "~/.emacs-custom-faces-tty.el")))
;;   (load custom-faces-file)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTES: this relies on certain internal implementation details of emacs22's
;; customize functionality:
;;
;; - that the custom-save-all function uses the return value of the
;;   (custom-file) function [which will be based on the value of the
;;   custom-file variable] to determine the location of the custom file, loads
;;   it into a buffer, then calls (custom-save-variables), then calls
;;   (custom-save-faces).
;;
;; - That the custom-save-variables and custom-save-faces functions modify and
;;   save the aforementioned buffer.
;;
;; - that the custom-file function uses the value of the custom-file variable
;;   as the basis of its return value.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 2008-02-04 r3133 initial version
;; 2008-02-04 r3134 add comments
;; 2008-02-05 r3138 fix a stoopid bug
;; 2008-02-14 r3191 factor out ad-activate stmts; add fns to enable/disable
;; 2008-02-18 r3229 emacswiki:CodingStyle mostly-compliance
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(defvar is-saving-custom-faces-file-only nil
  "If set to t, indicates that only face customizations are being written.
This variable is not intended to be set by users.")

(defvar is-saving-custom-variables-file-only nil
  "If set to t, indicates that only variable customizations are being written.
This variable is not intended to be set by users.")

(defvar custom-faces-file nil
  "File used for storing face customization information.
If set, variable `custom-file' only stores variable customizations.
If nil, all customizations are stored in variable `custom-file'.

The substring \"%s\" or \"%{window-system}\" will be replaced
with the value of variable `window-system', or `tty' if value of
variable `window-system' is NIL.

The substring \"%{theme}\" will be replaced with a
hyphen-separated list of themes from `custom-enabled-themes' or
the empty string.

\"%{-theme}\" or \"%{theme-}\" will be replaced with the same as
above, prefixed or suffixed with a hyphen if there are any themes
enabled.

The function `custom-faces-file' performs the above
substitutions.")

(defun custom-faces-file ()
  "Return the current custom faces file.

This function returns the result of performing the
`window-system` and `custom-enabled-themes' on the variable
`custom-faces-file', which see."
  (if (and (boundp 'custom-faces-file) custom-faces-file)
      (let* ((window-system-name (if window-system
				     (symbol-name window-system)
				   "tty"))
             (theme-name (if custom-enabled-themes
                             (mapconcat 'symbol-name custom-enabled-themes "-")))
             (hyphen-prefixed-theme-name (if theme-name (concat "-" theme-name)))
             (hyphen-suffixed-theme-name (if theme-name (concat theme-name "-")))
             (result custom-faces-file))
        (setq custom-faces-file (replace-regexp-in-string "%s"               window-system-name custom-faces-file nil 'literal))
        (setq custom-faces-file (replace-regexp-in-string "%{window-system}" window-system-name custom-faces-file nil 'literal))
        (setq custom-faces-file (replace-regexp-in-string "%{theme}"         (or theme-name "")                 custom-faces-file nil 'literal))
        (setq custom-faces-file (replace-regexp-in-string "%{-theme}"        (or hyphen-prefixed-theme-name "") custom-faces-file nil 'literal))
        (setq custom-faces-file (replace-regexp-in-string "%{theme-}"        (or hyphen-suffixed-theme-name "") custom-faces-file nil 'literal))
        custom-faces-file)))

;; (replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE
;; LITERAL SUBEXP START)

(defun setup-custom-faces-file ()
  "Advise customize to store variables and faces in different files.
See the `custom-faces-file' variable."
  (defadvice custom-file
    (around custom-file--custom-faces-file)
    (cond (is-saving-custom-faces-file-only
	   (if custom-faces-file
	       (let ((custom-file (custom-faces-file))) ad-do-it)
	     ad-do-it))
	  (is-saving-custom-variables-file-only ad-do-it)
	  (t ad-do-it)))
  (defadvice custom-save-variables
    (around custom-save-variables--custom-faces-file)
    (if (not is-saving-custom-faces-file-only) ad-do-it))
  (defadvice custom-save-faces
    (around custom-save-faces--custom-faces-file)
    (if (not is-saving-custom-variables-file-only) ad-do-it))
  (defadvice custom-save-all
    (around custom-save-all--custom-faces-file)
    (if custom-faces-file
	(progn
	  (let ((is-saving-custom-variables-file-only t))
	    ;; so custom-save-variables runs, but custom-save-faces doesn't.
	    ad-do-it)
	  (let ((is-saving-custom-faces-file-only t))
	    ;; so custom-save-faces runs, but custom-save-variables doesn't.
	    ad-do-it))
      ad-do-it))
  (activate-custom-faces-file-advice))

(defun activate-custom-faces-file-advice ()
  "Internal function."
  (ad-activate 'custom-file)
  (ad-activate 'custom-save-variables)
  (ad-activate 'custom-save-faces)
  (ad-activate 'custom-save-all))

(defun disable-custom-faces-file-advice ()
  "Disable custom-faces-file modifications to customize.
Not intended to be used except for debugging."
  (interactive)
  (ad-disable-regexp "--custom-faces-file$")
  (activate-custom-faces-file-advice))

(defun enable-custom-faces-file-advice ()
  "Disable custom-faces-file modifications to customize.
Not intended to be used except for debugging."
  (interactive)
  (ad-enable-regexp "--custom-faces-file$")
  (activate-custom-faces-file-advice))

(if (>= emacs-major-version 22)
    (setup-custom-faces-file))

(provide 'custom-faces-file)

;;; custom-faces-file.el ends here

