;;; viking-mode.el --- kill first, ask later

;; Copyright (C) 2016, T.v.Dein <tlinden@cpan.org>

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.02
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: kill delete
;; URL: https://github.com/tlinden/viking-mode
;; License: GNU General Public License >= 2

;;; Commentary:

;; Viking minor  mode enables you to  delete things at point  with one
;; key stroke at once. More and more will be deleted if you repeat the
;; key stroke.  As visual  feedback the  thing to  be deleted  will be
;; highlighted shortly.

;; The default key binding is C-d, but  you may also bind it to C-k or
;; whatever you wish.

;; If you press C-d the first time, the word at point will be deleted.
;; If you press it again, the remainder of the line from point will be
;; deleted. If pressed  again, the whole line, then  the paragraph and
;; finally the whole buffer will be deleted.

;; Like:
;; [keep pressing ctrl] C-d                  - del word
;;                      C-d C-d              - del line remainder
;;                      C-d C-d C-d          - del line
;;                      C-d C-d C-d C-d      - del paragraph
;;                      C-d C-d C-d C-d C-d  - del buffer

;; However, this only works when pressing the  key in a row. If you do
;; something  else in  between, it  starts from  scratch (i.e.  delete
;; word).

;;; Install:

;; To use, save viking-mode.el to a directory in your load-path.

;; Add something like this to your config:

;;    (require 'viking-mode)
;;    (add-hook 'text-mode-hook 'viking-mode)

;; or load it manually, when needed:

;;    M-x viking-mode

;; However, it's also possible to enable viking-mode globally:

;;    (viking-global-mode)

;;; Customize:

;; By  default viking-mode  doesn't really  delete things,  everything
;; remains available  for yanking in  the kill ring. However,  you may
;; turn it into berserk mode by setting 'viking-really-delete to t:

;;    (setq viking-really-delete t)

;; You can change the default key binding by:

;;    (define-key viking-mode-map (kbd "C-k") 'viking-kill-thing-at-point)

;; In case  you don't like  the default  key binding cascade,  you may
;; also use separate bindings for each kill function, e.g.:

;;    (define-key viking-mode-map (kbd "C-d") nil)   ;; turn C-d into a prefix-key
;;    (define-key viking-mode-map (kbd "C-d w") 'viking-kill-word)
;;    (define-key viking-mode-map (kbd "C-d r") 'viking-kill-line-from-point)
;;    (define-key viking-mode-map (kbd "C-d l") 'viking-kill-line)
;;    (define-key viking-mode-map (kbd "C-d p") 'viking-kill-paragraph)
;;    (define-key viking-mode-map (kbd "C-d a") 'viking-kill-buffer)

;; Also, the font face of the short highlight can be modified:

;;    M-x customize-face  (select viking-blink)

;; Or, modify all available viking-mode variables interactively with:

;;    M-x customize-group  (select viking-mode)

;;; Tips:

;; Besides, the primary function of viking-mode is viking-last-key-repeats,
;; which returns the number of times the last key have been pressed.
;; This can be used for other things as well, for example:

;;    (defun goto-begin()
;;      (interactive)
;;      (let* ((key-times (viking-last-key-repeats)))
;;        (cond
;;         ((eq key-times 3)
;;          (if mark-active
;;              (goto-char (point-min))
;;            (beginning-of-buffer))) 
;;         ((eq key-times 2)
;;          (if mark-active () (push-mark))
;;          (move-to-window-line 0)) 
;;         ((eq key-times 1)
;;          (beginning-of-line)))))

;;    (defun goto-end ()
;;      (interactive)
;;      (let* ((key-times (viking-last-key-repeats)))
;;        (cond
;;         ((eq key-times 3)
;;          (if mark-active
;;              (goto-char (point-max))
;;            (end-of-buffer))) 
;;         ((eq key-times 2)
;;          (if mark-active () (push-mark))
;;          (move-to-window-line -1)
;;          (end-of-line)) 
;;         ((eq key-times 1)
;;          (end-of-line)))))

;;    (global-set-key (kbd "<home>") 'goto-begin)
;;    (global-set-key (kbd "<end>")  'goto-end)

;; When you put this into your .emacs config, then you can do:
;; hit <home> once:  goto beginning of current line
;;          repeat:  goto beginning of current window
;;          repeat:  goto beginning of current buffer

;; (and the same with <end> in the other direction)

;;; Reporting Bugs:

;; Goto https://github.com/tlinden/viking-mode and file a new issue.




;;; Code:
;;;; Consts

(defconst viking-mode-version "0.02" "Viking Mode version.")


;;;; Customizable variables
;;;;; Fonts

(defface viking-blink
  '((t :inherit highlight))
  "Level 1."
  :group 'viking-mode)

;;;;; Modify behavior

(defcustom viking-blink-time 0.2
  "How long should viking highlight a region,
in seconds, specify milliseconds like this: 0.1"
  :group 'viking-mode)

(defcustom viking-really-delete nil
  "If set to t, really delete killed things. That is,
nothing will remain available via kill-ring once deleted.
The default is nil: keep deleted things in the kill-ring."
  :group 'viking-mode)



;;;; Functions
;;;;; Utilities

;; internal utility functions required by the kill functions

(defun viking--blink-region(begin end)
  "Blink a region shortly. It does this by highlighting the
region between 'begin and 'end using 'font-lock-viking-face
for 'viking-blink-time seconds."
  (interactive)
  (let* ((rh (make-overlay begin end)))
    (progn
      (overlay-put rh 'face 'viking-blink)
      (sit-for viking-blink-time t)
      (delete-overlay rh)
      )))

(defun viking--get-point (symbol &optional arg)
  "Returns the point by evaluating 'symbol, which
should be a point-moving function."
  (funcall symbol arg)
  (point)
  )

;; may be used for other purposes as well, see commentary 'Tips'
(defun viking-last-key-repeats ()
  "Returns how many times the last key has been pressed as integer."
  (interactive)
  (let* ((keys (reverse (append (recent-keys) nil))) ;; list of last keys pressed, last @0
         (pressed (car keys))                        ;; the very last key pressed, i.e. the one bound to this defun
         (times 0))                                  ;; how many times that key have been pressed
    (progn
      (catch 'nomore                     ;; don't iterate endless
        (dolist (k keys)                 ;; loop over the key list, (car keys) is the most recent, due to 'reverse above
          (if (equal pressed k)          ;; one more of the same key pressed in a row
              (setq times (+ times 1))   ;;   register
            (throw 'nomore t))))         ;; another key, break the loop and rerturn the count
      times)))

;;;;; kill/delete wrappers

(defun viking--kill-region (beg end)
  "Deletes or kills a region depending on 'viking-really-delete."
  (message "vkkill")
  (if viking-really-delete
      (delete-region beg end)
    (kill-region beg end)))

(defun viking--really-delete-line () ;; based on code by xah
  "kill-line without copy"
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )

(defun viking--kill-word-at-point ()
  "Kill word at point."
  (interactive)
  (let
      ((beg (viking--get-point 'backward-word 1))
       (end (viking--get-point 'forward-word 1)))
    (progn
      (viking--blink-region beg end)
      (viking--kill-region beg end)
      (message "word at point deleted"))))


(defun viking--kill-word-right ()
  "Kill word from line beginning to the right."
  (interactive)
  (let
      ((beg (point))
       (end (viking--get-point 'forward-word 1)))
    (viking--blink-region beg end)
    (viking--kill-region beg end)
    (message "word right of point deleted")))


;;;;; Public interactive kill functions

(defun viking-kill-word ()
  "Kill word at point"
  (interactive)
  (if (eq (point) (line-beginning-position))
      (viking--kill-word-right)
    (viking--kill-word-at-point)
    ))

(defun viking-kill-line ()
  "Kill line at point including newline."
  (interactive)
  (viking--blink-region (viking--get-point 'beginning-of-line 1)
                        (viking--get-point 'end-of-line 1))
  (move-beginning-of-line 1)
  (let ((k kill-whole-line))
    (progn
      (setq kill-whole-line t)  ;; temp enable
      (if viking-really-delete
          (viking--really-delete-line)
        (kill-line)
        )
      (setq kill-whole-line k)
      ))
  (message "line at point deleted"))

(defun viking-kill-line-from-point ()
  "Kill line from point to the right without newline."
  (interactive)
  (let ((beg (point))
        (end (viking--get-point 'end-of-line 1)))
    (progn
      (viking--blink-region beg end)
      (viking--kill-region beg end) 
      (message "line from point deleted"))))

(defun viking-kill-paragraph ()
  "Kill paragraph at point."
  (interactive)
  (let
      ((beg (viking--get-point 'backward-paragraph 1))
       (end (viking--get-point 'forward-paragraph 1)))
    (progn
      (viking--blink-region beg end)
      (viking--kill-region beg end)
      (message "paragraph at point deleted"))))

(defun viking-kill-buffer ()
  "Kill the whole buffer."
  (interactive)
  (let ((beg (point-min))
        (end (point-max)))
    (progn
      (viking--blink-region beg end)
      (viking--kill-region beg end)
      (message "buffer deleted"))))

;;;;; Primary key binding function

(defun viking-kill-thing-at-point()
  "Delete thing at point. Checks how many times the
calling key has been pressed and runs the appropriate
kill function then."
  (interactive)
  (let* ((key-times (viking-last-key-repeats)))
    (cond
     ((eq key-times 5) (viking-kill-buffer)) 
     ((eq key-times 4) (viking-kill-paragraph)) 
     ((eq key-times 3) (viking-kill-line))
     ((eq key-times 2) (viking-kill-line-from-point))
     ((eq key-times 1) (viking-kill-word))
     )
    ))

;;;; Interface
;;;###autoload

;; the minor mode, can be enabled by major mode via hook or manually
(define-minor-mode viking-mode "kill first, ask later"
  :lighter " V"
  :group 'viking-mode
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-d") 'viking-kill-thing-at-point)
            map))

;; just in case someone wants to use it globally
(define-global-minor-mode viking-global-mode
  viking-mode
  (lambda () (viking-mode t)
    ))

;; un-*ing-believable: I'm done *g* 
(provide 'viking-mode)


;;; viking-mode.el ends here
