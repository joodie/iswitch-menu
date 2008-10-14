;;; iswitch-menu.el --- Use iswitch to access menus

;; Copyright (C) 2008  Joost Diepenmaat

;; Author: Joost Diepenmaat <joost@zeekat.nl>
;; Keywords: matching, frames, terminals, menus

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; SYNOPSIS
;;
;; This code replaces tmm-prompt with something a lot more convenient.
;; Prompted by the horrible navigation menus in the otherwise
;; *excellent* emacs-rails library, this code should make using menus
;; without a mouse or X easy on the brain and fingers.
;;
;; to use: put the following in your init file and make sure this
;; file is in your load-path:
;;
;; (require 'iswitch-menu)
;;
;; you can then access the console menu by running
;; M-x tmm-menubar
;; note that this will work even if you're using a GUI menu bar
;;
;; CUSTOMIZATION
;;
;; You can customize the menus using the iswitch-menu customization
;; group:
;;
;; M-x customize-group <ENTER> iswitch-menu <ENTER>
;;
;; you can toggle iswitch-menu-override-tmm-prompt if you want to use
;; iswitch-menu-prompt only for specific modes or commands.
;;
;; REPORTING BUGS
;;
;; This code should work as advertized.  If you find any situations
;; where it doesn't work, try the the latest version from
;; http://github.com/joodie/iswitch-menu/

;; If that doesn't work either, please contact the author.  If any
;; errors occur during the creation or executiong of a particular
;; menu, please provide the output of M-x iswitch-menu-report as run
;; immediately after the error.
;;
;; USING THIS CODE WITH EMACS-RAILS
;;
;; To use with emacs-rails navigation: same as above, and switch on
;; the Rails Always Use Text Menus option in the rails customization
;; group.
;;
;; Alternatively, if you don't like to globally override tmm-prompt,
;; get a version of emacs-rails with configurable text menus, like
;; this one: http://github.com/remvee/emacs-rails/tree/master and
;; switch on rails-always-use-text-menu and set
;; rails-text-menu-function to #'iswitch-menu-prompt
;; 
;;; History:
;;
;; 2008/10/14 - Added support for keymaps in function cells instead
;;   of just value cells.
;;
;; 2008/10/12 - Added some fixes that should make byte-compiling work
;;   and make the menu switch on iswitchb-mode temporarily if needed.
;;
;; Release 1.0 - 2008/10/04 - Set last-command-event when selecting
;;   menu items this means "Paste from kill menu" now works and
;;   possibly fixes some other issues.  As far as I can tell, the code
;;   should now work for any menu that works with tmm-prompt or
;;   x-popup-menu.
;;
;; 2008/10/03 - Evaluated titles in extended menu items now work. As
;;   far as I can tell, this means the menu/keymap parsing code is now
;;   complete.
;;
;; 2008/09/24 - More inline documentation; more conformance to the
;;   elisp conventions.
;;
;; 2008/09/23 - and earlier: less documented versions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'cl)
(require 'iswitchb)
(require 'tmm)

(defgroup iswitch-menu nil
  "keyboard driven menus based on iswitchb"
  :group 'menu
  :prefix "iswitch-menu")

(defcustom iswitch-menu-checked-toggle-mark "(*)"
  "A string indicating a menu item is a checked toggle item."
  :group 'iswitch-menu
  :type 'string)

(defcustom iswitch-menu-unchecked-toggle-mark "( )"
  "A string indicating a menu item is an unchecked toggle item."
  :group 'iswitch-menu
  :type 'string)

(defcustom iswitch-menu-checked-radio-mark "[*]"
  "A string indicating a menu item is a checked radio item."
  :group 'iswitch-menu
  :type 'string)

(defcustom iswitch-menu-unchecked-radio-mark "[ ]"
  "A string indicating a menu item is an unchecked radio item."
  :group 'iswitch-menu
  :type 'string)

(defcustom iswitch-menu-override-tmm-prompt t
  "If true, override `tmm-prompt' to use `iswitch-menu-prompt'.
this means all `text-mode' menus will use iswitch-menu"
  :group 'iswitch-menu
  :type 'boolean)

;; this will store debug info
(defvar iswitch-menu-parse-error :no-error)
(defvar iswitch-menu-captured-keymap :none)
(defvar iswitch-menu-last-chosen :none)
(defvar iswitch-menu-last-single-prompt :none)

;; adapted from iswitchb.el, Kin Cho
(defun iswitch-menu-single-prompt (prompt items)
  "Display a non-nested menu (using `iswitchb').
PROMPT is a string and ITEMS is an alist of options ((TITLE
. SOMETHING) ...).  Return the selected cons."
  (let ((mode-on iswitchb-mode))
    (unless mode-on
      (iswitchb-mode 1))
    (unwind-protect
	(progn
	  (setq iswitch-menu-last-single-prompt items)
	  (let ((iswitchb-make-buflist-hook
		 (lambda ()
		   (setq iswitchb-temp-buflist (mapcar #'car items)))))
	    (let ((r (assoc (iswitchb-read-buffer prompt) items)))
	      (setq last-command-event (car r)) ; this is used by "edit > paste from kill menu" & others
	      r)))
      (unless mode-on
	(iswitchb-mode -1)
	nil))))

(defun iswitch-menu-nested-prompt (prompt items)
  "Display a possibly nested menu (using `iswitchb').
PROMPT is a string and ITEMS is an alist consisting of (TITLE
. DEFINITION) pairs.  DEFINITIONs are cons cells
containing (:result . RESULT) or (:menu . ALIST), where RESULT is
returned if the user chooses it, and ALIST is a submenu
definition"
  (let ((chosen (iswitch-menu-single-prompt (concat prompt " > ") items)))
    (when chosen
	(setq iswitch-menu-last-chosen chosen)
	(case (cadr chosen)
	  (:result (cddr chosen))
	  (:menu (iswitch-menu-nested-prompt (concat prompt " > " (car chosen)) (cddr chosen)))
	  (t chosen)))))

;; see elisp manual section "Extended Menu Items"
(defun iswitch-menu-parse-extended-menu-item (menu-item)
  "Convert an extended MENU-ITEM into a form suitable for `iswitch-menu'.
See also the elisp manual section on Extended Menu Items."
  (when (and (cdr menu-item) ;; skip non-selectable menu-items
	     (consp (cdr menu-item))
	     (consp (cddr menu-item)))
    (let* ((title (cadr menu-item))
	   (binding (caddr menu-item))
	   (props (if (keywordp (cadddr menu-item))
		      (cdddr menu-item)
		      (cddddr menu-item)))
	   (button (getf props :button)))
      (unless (stringp title)
	(setq title (eval title)))
      (if (getf props :filter)
	  (setq binding (funcall (getf props :filter) binding)))
      (if (keymapp binding)
	  (setq binding (cons :menu (iswitch-menu-parse-keymap binding)))
	(setq binding (cons :result binding)))
      (if (and (eval (getf props :enable t))
	       (eval (getf props :visible t)))
	  (if button
	      (let* ((ticked (eval (cdr button)))
		     (mark (if (eql :toggle (car button))
			       (if ticked
				   iswitch-menu-checked-toggle-mark
				 iswitch-menu-unchecked-toggle-mark)
			     (if ticked
				 iswitch-menu-checked-radio-mark
			       iswitch-menu-unchecked-radio-mark))))
		(cons (concat title " " mark) binding))
	    (cons title binding))
	nil))))



(defun iswitch-menu-parse-menu-item (menu-item)
  "Convert a MENU-ITEM into a form suitable for `iswitch-menu'."
  (when (and menu-item
	     (consp menu-item)
	     (cdr menu-item))
    (cond
     ((eql (car menu-item) 'menu-item)
      (iswitch-menu-parse-extended-menu-item menu-item))
     ((stringp (car menu-item)) ;; simple menu item
      (let ((binding (if (and (consp (cdr menu-item))
			      (stringp (cadr menu-item)) ;; help string
			      (cddr menu-item)) ;; has a real binding
			 (cddr menu-item)
		       (cdr menu-item))))
	(cons (car menu-item) (if (keymapp binding)
			     (cons :menu (iswitch-menu-parse-keymap binding))
			   (cons :result binding)))))
     (t nil))))

(defun iswitch-menu-parse-keymap (keymap)
 "Convert a KEYMAP menu into a form suitable for `iswitch-menu'."
  (let ((orig-keymap keymap))
    (if (and (atom keymap)
	     (symbolp keymap)
	     (keymapp keymap))
	(if (boundp keymap)
	    (setq keymap (symbol-value keymap))
	    (if (fboundp keymap)
		(setq keymap (symbol-function keymap)))))
    (if (keymapp keymap)
	 (let ((result))
	   (map-keymap (lambda (key def)
			 (let ((p (iswitch-menu-parse-menu-item def)))
			   (if p
			       (setq result (cons p result))))) keymap)
	   (cons (if (consp (cadr keymap))
		     (cadr (cadr keymap))
		   (cadr keymap)) result)
	   (nreverse result))
      (remove-if #'null (mapcar #'iswitch-menu-parse-menu-item keymap)))))


(defun iswitch-menu-prompt (menu &rest ignore)
  "A drop-in replacement for `tmm-prompt' and `x-popup-menu' using `iswitchb'.
Should make using MENUs from the console / keyboard faster and
more comfortable."
  (setq iswitch-menu-captured-keymap menu)
  (if (or (keymapp menu)
	  (keymapp (car menu)))
      (progn (let ((r (iswitch-menu-nested-prompt "Menu" (iswitch-menu-parse-keymap menu))))
	       (call-interactively (cond ((symbolp r) r)
					 ((and (consp r) (consp (car r)) (eql 'lambda (cadr r))) (cdr r))))))
    (iswitch-menu-nested-prompt (caadr menu) (remove-if #'null (mapcar #'iswitch-menu-parse-menu-item (cdadr menu))))))


(defadvice tmm-prompt (around use-iswitch-menu-prompt (menu &rest rest))
  "Call either `tmm-prompt' or `iswitch-menu-prompt'.
Which is called depends on the `iswitch-menu-override-tmm-prompt'
customization setting"
  (if iswitch-menu-override-tmm-prompt
      (iswitch-menu-prompt menu)
    ad-do-it))

(ad-enable-advice #'tmm-prompt 'around 'use-iswitch-menu-prompt)
(ad-activate #'tmm-prompt)

(defun iswitch-menu-report ()
  "Show debug information for `iswitch-menu'."
  (interactive)
  (let ((buf (generate-new-buffer "*ISWITCH-MENU-DEBUG*")))
    (display-buffer buf)
    (set-buffer buf)
    (insert "iswitch-menu debug info:\n\n")
    (dolist (s '(iswitch-menu-parse-error iswitch-menu-captured-keymap iswitch-menu-last-chosen iswitch-menu-last-single-prompt))
      (insert (symbol-name s) ": " (pp-to-string (symbol-value s)) "\n\n"))))

(provide 'iswitch-menu)
;;; iswitch-menu.el ends here
