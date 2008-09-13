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

;; This code replaces tmm-prompt with something a lot more convenient.
;; Prompted by the horrible navigation menus in the otherwise
;; *excellent* emacs-rails library, this code should make using menus
;; without a mouse or X easy on the brain and fingers.

;; to use with emacs-rails: require this code and switch on the Rails
;; Always Use Text Menus option in the rails customization group.

;;; Code:
(require 'tmm)
(require 'iswitchb)

;; adapted from iswitchb.el, Kin Cho

(defun iswitch-menu-single-prompt (prompt items)
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist (mapcar #'car items)))))
    (assoc (iswitchb-read-buffer prompt) items)))

(defun iswitch-menu-nested-prompt (prompt items)
  (message "%s" items)
  (let ((chosen (iswitch-menu-single-prompt (concat prompt " > ") items)))
    (if (consp (cdr chosen))
	(iswitch-menu-nested-prompt (concat prompt " > " (car chosen)) (cdr chosen))
	chosen)))

(defun iswitch-menu-parse-keymap (keymap)
  (error "Not yet implemented")
  (if (eql 'keymap (car keymap)) ; single keymap
      (let ((title)
	    (list))
	(dolist (item (cdr keymap))
	  (if (and (not title)
		   (stringp item))
	      (setq title item)
	    (setq list (cons list (iswitch-menu-parse-keymap item)))))
	(list title (nreverse list)))
    keymap))

(defvar captured-keymap t)

(defun iswitch-menu-prompt (menu &rest ignored)
  "A drop-in replacement for tmm-prompt and x-popup-menu using iswitchb semantics
and technology. Should make using menus from the console / keyboard faster and
more comfortable."
  (if (or (keymapp menu)
	  (keymapp (car menu)))
      (setq captured-keymap menu)
      (iswitch-menu-stored-tmm-prompt menu)
;      (cdr (apply #'iswitch-menu-nested-prompt (iswitch-menu-parse-keymap menu)))
    (cdr (iswitch-menu-nested-prompt (caadr menu) (cdadr menu)))))

(defun iswitch-menu-stored-tmm-prompt nil)

(defun iswitch-menu-toggle ()
  (interactive)
  (cond (#'iswitch-menu-stored-tmm-prompt 
	 (fset #'tmm-prompt #'iswich-menu-stored-tmm-prompt)
	 (fset #'iswitch-menu-stored-tmm-prompt nil))
	(t
	 (fset #'iswitch-menu-stored-tmm-prompt #'tmm-prompt)
	 (fset #'tmm-prompt #'iswitch-menu-prompt))))

(defun iswitch-menu (&optional s)
  (interactive)
  (if s
    (if (> s 1)
	(iswitch-menu-on)
      (iswitch-menu-off))
    (iswitch-menu-toggle)))

(provide 'iswitch-menu)
;;; iswitch-menu.el ends here
