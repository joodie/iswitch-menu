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



;; This code replaces tmm-prompt with something a lot more convenient.
;; Prompted by the horrible navigation menus in the otherwise
;; *excellent* emacs-rails library, this code should make using menus
;; without a mouse or X easy on the brain and fingers.
;;
;; to use: put the following in your init file and make sure this
;; file is in your load-path:
;;
;; (require 'iswitch-menu)
;; (iswitch-menu-override-tmm-prompt)
;;
;; you can then access the console menu by running
;; M-x tmm-menubar
;; note that this will work even if you're using a GUI menu bar
;;
;; to use with emacs-rails navigation: same as above, and switch on
;; the Rails Always Use Text Menus option in the rails customization
;; group.


;; adapted from iswitchb.el, Kin Cho
(defun iswitch-menu-single-prompt (prompt items)
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist (mapcar #'car items)))))
    (assoc (iswitchb-read-buffer prompt) items)))

(defun iswitch-menu-nested-prompt (prompt items)
  (let ((chosen (iswitch-menu-single-prompt (concat prompt " > ") items)))
    (if (and (consp (cdr chosen))
	     (not (eql 'lambda (cadr chosen))))
	(iswitch-menu-nested-prompt (concat prompt " > " (car chosen)) (cdr chosen))
	chosen)))

(defun iswitch-menu-eventp (thing)
  (and (not (keymapp thing))
       (or (symbolp thing)
	   (and (consp thing)
		(eql 'lambda (car thing))))))

;; parse a single keymap key definition
;; (which can be a keymap itself)
;; TODO: test for visibility/enabled-ness
(defun iswitch-menu-parse-keymap-entry (def)
  (when (consp def)
    (setq def (if (eql 'menu-item (car def))
		  (cdr def)
		def))
    (let ((title (if (stringp (car def)) 
		     (car def)
		   (eval (car def)))))
      (cond
       ;; ("--")
       ((not (cdr def))
	nil)
       ;; ("Title" . definition)
       ((iswitch-menu-eventp (cdr def))
	(cons title (cdr def)))
      ;; ("Title" longer definition)
       ((and (consp (cdr def))
	     (not (keymapp (cdr def)))
	     (iswitch-menu-eventp (cadr def)))
	(cons title (cadr def)))
       ;; ("Title" "Description" . def)
       ((and (consp (cdr def))
	     (or (stringp (cadr def))
		 (and (consp (cadr def))
		      (null (caadr def))))
	     (iswitch-menu-eventp (cddr def)))
	(cons title (cddr def)))
       ;; nested keymaps
       ((keymapp (cdr def))
	(cons title (iswitch-menu-parse-keymap (cdr def))))
       ((keymapp (cadr def))
	(cons title (iswitch-menu-parse-keymap (cadr def))))
       ;;
       (t
	(error "iswitch-menu error: can't handle keymap definition %s" def))))))

(defun iswitch-menu-parse-keymap (keymap)
  (if (symbolp keymap)
      nil ;; keymap passed as symbol - we need some fix for this - 
    (let ((result))
      (map-keymap (lambda (key def)
		    (let ((p (iswitch-menu-parse-keymap-entry def)))
		      (if p
			  (setq result (cons p result))))) keymap)			
      (cons (if (consp (cadr keymap))
		(cadr (cadr keymap))
	      (cadr keymap)) result)
      (nreverse result))))

;; this keeps the last requested keymap sent to
;; iswitch-menu-prompt. useful for debugging
(defvar iswitch-menu-captured-keymap t)

(defun iswitch-menu-prompt (menu &rest ignored)
  "A drop-in replacement for tmm-prompt and x-popup-menu using
iswitchb semantics and technology. Should make using menus from
the console / keyboard faster and more comfortable."
  (if (or (keymapp menu)
	  (keymapp (car menu)))
      (progn (setq iswitch-menu-captured-keymap menu)
	     ;; apparently, tmm-prompt calls events from keymaps
	     (let ((r (cdr (iswitch-menu-nested-prompt "Menu" (iswitch-menu-parse-keymap menu)))))
	       (call-interactively r)))
    (cdr (iswitch-menu-nested-prompt (caadr menu) (cdadr menu)))))

(defvar old-tmm-prompt-set 0)
(defun old-tmm-prompt ())

(defun iswitch-menu-override-tmm-prompt ()
  "Install iswitch-menu-prompt over tmm-prompt. this means all
x-popup-menu calls will use iswitch-menu-prompt instead. There is
currently no way to revert this command"
  (interactive)
  (when (= old-tmm-prompt-set 0)
    (require 'tmm)
    (fset #'old-tmm-prompt #'tmm-prompt)
    (setq old-tmm-prompt-set 1)
    (fset #'tmm-prompt #'iswitch-menu-prompt)))


(provide 'iswitch-menu)
;;; iswitch-menu.el ends here
