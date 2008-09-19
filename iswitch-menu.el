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
;; (iswitch-menu-override-tmm-prompt)
;;
;; you can then access the console menu by running
;; M-x tmm-menubar
;; note that this will work even if you're using a GUI menu bar
;;
;; REPORTING BUGS
;;
;; This code is still under construction. If you find any situations
;; where it doesn't work, trye the the latest version from
;; http://github.com/joodie/iswitch-menu/

;; If that doesn't work either, please contact the author. If any
;; errors occur during the creation or executiong of a particular
;; menu, please provide the output of M-x iswitch-menu-report as run
;; immediately after the error.
;;
;; USING THIS CODE WITH EMACS-RAILS
;;
;; to use with emacs-rails navigation: same as above, and switch on
;; the Rails Always Use Text Menus option in the rails customization
;; group.
;;
;; alternatively, if you don't like to globally override tmm-prompt,
;; get a version of emacs-rails with configurable text menus, like
;; this one: http://github.com/remvee/emacs-rails/tree/master and
;; switch on rails-always-use-text-menu and set
;; rails-text-menu-function to #'iswitch-menu-prompt
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;; this will store debug info
(defvar iswitch-menu-last-keydef-error :no-error)
(defvar iswitch-menu-parse-error :no-error)
(defvar iswitch-menu-captured-keymap :none)
(defvar iswitch-menu-last-chosen :none)

;; adapted from iswitchb.el, Kin Cho
(defun iswitch-menu-single-prompt (prompt items)
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist (mapcar #'car items)))))
    (assoc (iswitchb-read-buffer prompt) items)))

(defun iswitch-menu-nested-prompt (prompt items)
  (let ((chosen (iswitch-menu-single-prompt (concat prompt " > ") items)))
    (if (and (consp (cdr chosen))
	     (not (eql 'lambda (cadr chosen)))
	     (> (length (cdr chosen)) 1))
	(iswitch-menu-nested-prompt (concat prompt " > " (car chosen)) (cdr chosen))
      (setq iswitch-menu-last-chosen chosen)
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
      (if (or (not (getf def :enable))
	      (eval (getf def :enable)))
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
	   ;; ("Title" (stuff) longer definition)
	   ((and (consp (cdr def))
		 (not (keymapp (cadr def)))
		 (iswitch-menu-eventp (cddr def)))
	    (cons title (cddr def)))
	   ;; ("Title" "Description" . def)
	   ((and (consp (cdr def))
		 (or (stringp (cadr def))
		     (and (consp (cadr def))
			  (null (caadr def))))
		 (iswitch-menu-eventp (cddr def)))
	    (cons title (cddr def)))
	   ;; ("Title" "Description" (stuff) . def)
	   ((and (consp (cdr def))
		 (and (stringp (cadr def))
		      (and (consp (cddr def))
			   (consp (caddr def))
			   (not (eql 'lambda (caaddr def)))
			   (iswitch-menu-eventp (cdddr def)))))
	    (cons title (cdddr def)))
	   ;; nested keymaps
	   ((keymapp (cdr def))
	    (cons title (iswitch-menu-parse-keymap (cdr def))))
	   ((keymapp (cadr def))
	    (cons title (iswitch-menu-parse-keymap (cadr def))))
	   ;;
	   (t
	    (message "s" (pp def))
	    (setq iswitch-menu-last-keydef-error def)
	    (error "iswitch-menu error: can't handle keymap definition")))))))

(defun iswitch-menu-parse-keymap (keymap)
  (let ((orig-keymap keymap))
    (if (and (symbolp keymap)
	     (keymapp keymap))
	(setq keymap (symbol-value keymap)))
    (if (keymapp keymap)
	(let ((result))
	  (map-keymap (lambda (key def)
			(let ((p (iswitch-menu-parse-keymap-entry def)))
			  (if p
			      (setq result (cons p result))))) keymap)			
	  (cons (if (consp (cadr keymap))
		    (cadr (cadr keymap))
		  (cadr keymap)) result)
	  (nreverse result))
      ;; not a valid keymap result. the only sitution I've run into is
      ;; with yank-menu on my system.
      (setq iswitch-menu-parse-error (list :orig orig-keymap :interpolated keymap))
      nil)))


(defun iswitch-menu-prompt (menu &rest ignored)
  "A drop-in replacement for tmm-prompt and x-popup-menu using
iswitchb semantics and technology. Should make using menus from
the console / keyboard faster and more comfortable."
  (setq iswitch-menu-captured-keymap menu)
  (if (or (keymapp menu)
	  (keymapp (car menu)))
      (progn (let ((r (cdr (iswitch-menu-nested-prompt "Menu" (iswitch-menu-parse-keymap menu)))))
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

(defun iswitch-menu-report ()
  (interactive)
  (let ((buf (generate-new-buffer (generate-new-buffer-name "*ISWITCH-MENU-DEBUG*"))))
    (display-buffer buf)
    (set-buffer buf)
    (insert "iswitch-menu debug info:\n\n")
    (dolist (s '(iswitch-menu-last-keydef-error iswitch-menu-parse-error iswitch-menu-captured-keymap iswitch-menu-last-chosen))
      (insert (symbol-name s) ": " (pp-to-string (symbol-value s)) "\n\n"))))

 


(provide 'iswitch-menu)
;;; iswitch-menu.el ends here
