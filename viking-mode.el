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

;;      Version: 0.01
;;       Author: T.v.Dein <tlinden@cpan.org>
;;   Maintainer: T.v.Dein <tlinden@cpan.org>
;;      Created: May 2016
;;     Keywords: kill delete
;;     Homepage: http://www.daemon.de/Viking
;;   Repository: https://github.com/tlinden/viking-mode
;;      License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;;; Commentary

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
;; [keep pressing ctrl] C-d                  - word
;;                      C-d C-d              - line remainder
;;                      C-d C-d C-d          - line
;;                      C-d C-d C-d C-d      - paragraph
;;                      C-d C-d C-d C-d C-d  - buffer

;; However, this only works when pressing the  key in a row. If you do
;; something  else in  between, it  starts from  scratch (i.e.  delete
;; word).

;; To use, save viking-mode.el to a directory in your load-path.

;; Add something like this to your config:

;;    (require 'viking-mode)
;;    (add-hook 'text-mode-hook 'turn-on-viking-mode)

;; or load it manually

;;    M-x viking-mode

;; However, it's also possible to enable viking-mode globally:

;;    (viking-global-mode)

;; By  default viking-mode  doesn't really  delete things,  everything
;; remains available  for yanking in  the kill ring. However,  you may
;; turn it into berserk mode by setting 'viking-really-delete to t:

;;    (setq viking-really-delete t)

;; You can change the default key binding by:

;;    (define-key viking-mode-map (kbd "C-k") 'vk/kill-thing-at-point)

;; 

;; In case  you don't like  the default  key binding cascade,  you may
;; also use separate bindings for each kill function, e.g.:

;;    (define-key viking-mode-map (kbd "C-d") nil)   ;; turn C-d into a prefix-key
;;    (define-key viking-mode-map (kbd "C-d w") 'vk/kill-word)
;;    (define-key viking-mode-map (kbd "C-d r") 'vk/kill-line-from-point)
;;    (define-key viking-mode-map (kbd "C-d l") 'vk/kill-line)
;;    (define-key viking-mode-map (kbd "C-d p") 'vk/kill-paragraph)
;;    (define-key viking-mode-map (kbd "C-d a") 'vk/kill-buffer)

;; Also, the font face of the short highlight can be modified:

;;    M-x customize-face  (select viking-blink)

;; Or, modify all available viking-mode variables interactively with:

;;    M-x customize-group  (select viking-mode)

;; Besides, the primary function of viking-mode is vk/last-key-repeats,
;; which returns the number of times the last key have been pressed.
;; This can be used for other things as well, for example:

;;    (defun goto-begin()
;;      (interactive)
;;      (let* ((key-times (vk/last-key-repeats)))
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
;;      (let* ((key-times (vk/last-key-repeats)))
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





;;; Code
;;;; Customizable variables
;;;;; Fonts

(defface viking-blink
  '((t :inherit highlight))
  "Level 1."
  :group 'viking-mode)

;;;;; Modify behavior

(defcustom viking-blink-time 0.2
  "How long should 'vk/blink-region highligh a region,
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

(defun vk/blink-region(begin end)
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

(defun vk/get-point (symbol &optional arg)
  "Returns the point by evaluating 'symbol, which
should be a point-moving function."
  (funcall symbol arg)
  (point)
  )

(defun vk/last-key-repeats ()
  "Returns how many times the last key has been pressed as integer."
  ;; FIXME: should be possible with just counting '(recent-keys)
  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         (key3 (if (> len 2) (elt keys (- len 3)) nil))
         (key4 (if (> len 3) (elt keys (- len 4)) nil))
         (key5 (if (> len 4) (elt keys (- len 5)) nil))
         (key-equal-1 (equal key1 key2))
         (key-equal-2 (and key-equal-1 (equal key2 key3)))
         (key-equal-3 (and key-equal-2 (equal key3 key4)))
         (key-equal-4 (and key-equal-3 (equal key4 key5))))
    (cond (key-equal-4 5)
          (key-equal-3 4)
          (key-equal-2 3)
          (key-equal-1 2)
          (t 1)
          )))

;;;;; kill/delete wrappers

(defun vk/kill-region (beg end)
  "Deletes or kills a region depending on 'viking-really-delete."
  (message "vkkill")
  (if viking-really-delete
      (delete-region beg end)
    (kill-region beg end)))

(defun vk/really-delete-line () ;; based on code by xah
  "kill-line without copy"
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )

;;;;; Interactive kill functions

(defun vk/kill-word-at-point ()
  "Kill word at point."
  (interactive)
  (let
      ((beg (vk/get-point 'backward-word 1))
       (end (vk/get-point 'forward-word 1)))
    (progn
      (vk/blink-region beg end)
      (vk/kill-region beg end)
      (message "word at point deleted"))))


(defun vk/kill-word-right ()
  "Kill word to the right"
  (interactive)
  (let
      ((beg (point))
       (end (vk/get-point 'forward-word 1)))
    (vk/blink-region beg end)
    (vk/kill-region beg end)
    (message "word right of point deleted")))

(defun vk/kill-word ()
  "Kill word"
  (interactive)
  (if (eq (point) (line-beginning-position))
      (vk/kill-word-right)
    (vk/kill-word-at-point)
    ))

(defun vk/kill-line ()
  "Kill line at point including newline."
  (interactive)
  (vk/blink-region (vk/get-point 'beginning-of-line 1) (vk/get-point 'end-of-line 1))
  (move-beginning-of-line 1)
  (let ((k kill-whole-line))
    (progn
      (setq kill-whole-line t)  ;; temp enable
      (if viking-really-delete
          (vk/really-delete-line)
        (kill-line)
        )
      (setq kill-whole-line k)
      ))
  (message "line at point deleted"))

(defun vk/kill-line-from-point ()
  "Kill line from point to the right without newline."
  (interactive)
  (let ((beg (point))
        (end (vk/get-point 'end-of-line 1)))
    (progn
      (vk/blink-region beg end)
      (vk/kill-region beg end) 
      (message "line from point deleted"))))

(defun vk/kill-paragraph ()
  "Kill paragraph at point."
  (interactive)
  (let
      ((beg (vk/get-point 'backward-paragraph 1))
       (end (vk/get-point 'forward-paragraph 1)))
    (progn
      (vk/blink-region beg end)
      (vk/kill-region beg end)
      (message "paragraph at point deleted"))))

(defun vk/kill-buffer ()
  "Kill the whole buffer."
  (interactive)
  (let ((beg (point-min))
        (end (point-max)))
    (progn
      (vk/blink-region beg end)
      (vk/kill-region beg end)
      (message "buffer deleted"))))

;;;;; Primary key binding function

(defun vk/kill-thing-at-point()
  "Delete thing at point. Checks how many times the
calling key has been pressed and runs the appropriate
kill function then."
  (interactive)
  (let* ((key-times (vk/last-key-repeats)))
    (cond
     ((eq key-times 5) (vk/kill-buffer)) 
     ((eq key-times 4) (vk/kill-paragraph)) 
     ((eq key-times 3) (vk/kill-line))
     ((eq key-times 2) (vk/kill-line-from-point))
     ((eq key-times 1) (vk/kill-word))
     )
    ))

;;;; Interface
;;;###autoload

;; the minor mode, can be enabled by major mode via hook or manually
(define-minor-mode viking-mode "kill first, ask later"
  :lighter " V"
  :group 'viking-mode
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-d") 'vk/kill-thing-at-point)
            map))

;; just in case someone wants to use it globally
(define-global-minor-mode viking-global-mode
  viking-mode
  (lambda () (viking-mode t)
    ))

;; be nice and provide a toggle
(defun turn-on-viking-mode ()
  "turn viking-mode on"
  (interactive)
  (viking-mode 1))

;; un-*ing-believable: I'm done *g* 
(provide 'viking-mode)


;;; viking-mode.el ends here
