# viking-mode
Kill first, ask later - an emacs mode for killing things quickly

# Demo

![demo](http://www.daemon.de/idisk/Misc/viking-mode-demo.gif)

# Introduction

Viking minor mode  enables you to delete things at  point with one key
stroke at once.  More  and more will be deleted if  you repeat the key
stroke. As visual feedback the thing to be deleted will be highlighted
shortly.

The default  key binding is C-d,  but you may  also bind it to  C-k or
whatever you wish.

If you press  C-d the first time,  the word at point  will be deleted.
If you press  it again, the remainder  of the line from  point will be
deleted.  If pressed  again, the  whole line,  then the  paragraph and
finally the whole buffer will be deleted.

Like:

    [keep pressing ctrl] C-d                  - word
                         C-d C-d              - line remainder
                         C-d C-d C-d          - line
                         C-d C-d C-d C-d      - paragraph
                         C-d C-d C-d C-d C-d  - buffer

However, this  only works when  pressing the key in  a row. If  you do
something else in between, it starts from scratch (i.e.  delete word).

# Install

To use, save viking-mode.el to a directory in your load-path.

Add something like this to your config:

    (require 'viking-mode)
    (add-hook 'text-mode-hook 'viking-mode)

or load it manually

    M-x viking-mode

However, it's also possible to enable viking-mode globally:

    (viking-global-mode)

# Customize

By  default  viking-mode  doesn't  really  delete  things,  everything
remains available for yanking in the  kill ring. However, you may turn
it into berserk mode by setting 'viking-really-delete to t:

    (setq viking-really-delete t)

You can change the default key binding by:

    (define-key viking-mode-map (kbd "C-k") 'viking-kill-thing-at-point)

In case you  don't like the default key binding  cascade, you may also
use separate bindings for each kill function, e.g.:

    (define-key viking-mode-map (kbd "C-d") nil)   turn C-d into a prefix-key
    (define-key viking-mode-map (kbd "C-d w") 'viking-kill-word)
    (define-key viking-mode-map (kbd "C-d r") 'viking-kill-line-from-point)
    (define-key viking-mode-map (kbd "C-d l") 'viking-kill-line)
    (define-key viking-mode-map (kbd "C-d p") 'viking-kill-paragraph)
    (define-key viking-mode-map (kbd "C-d a") 'viking-kill-buffer)

Also, the font face of the short highlight can be modified:

    M-x customize-face  (select viking-blink)

Or, modify all available viking-mode variables interactively with:

    M-x customize-group  (select viking-mode)

# Tips

Besides, the primary function of viking-mode is viking-last-key-repeats,
which returns the number of times the last key have been pressed.
This can be used for other things as well, for example:

    (defun goto-begin()
    (interactive)
      (let* ((key-times (viking-last-key-repeats)))
        (cond
         ((eq key-times 3)
          (if mark-active
              (goto-char (point-min))
            (beginning-of-buffer))) 
         ((eq key-times 2)
          (if mark-active () (push-mark))
          (move-to-window-line 0)) 
         ((eq key-times 1)
          (beginning-of-line)))))
    
    (defun goto-end ()
      (interactive)
      (let* ((key-times (viking-last-key-repeats)))
        (cond
         ((eq key-times 3)
          (if mark-active
              (goto-char (point-max))
            (end-of-buffer))) 
         ((eq key-times 2)
          (if mark-active () (push-mark))
          (move-to-window-line -1)
          (end-of-line)) 
         ((eq key-times 1)
          (end-of-line)))))
    
    (global-set-key (kbd "<home>") 'goto-begin)
    (global-set-key (kbd "<end>")  'goto-end)

When you put this into your .emacs config, then you can do:

   hit <home> once:  goto beginning of current line
            repeat:  goto beginning of current window
            repeat:  goto beginning of current buffer
   
(and the same with <end> in the other direction)

# Reporting Bugs

Goto https://github.com/tlinden/viking-mode and file a new issue.

# Meta

Copyright (C) 2016, T.v.Dein <tlinden@cpan.org>

This file is NOT part of Emacs.

This program is  free software; you can redistribute  it and/or modify
it under the  terms of the GNU General Public  License as published by
the Free Software Foundation; either version  2 of the License, or (at
your option) any later version.

This program  is distributed in the  hope that it will  be useful, but
WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
General Public License for more details.

You should  have received  a copy  of the  GNU General  Public License
along  with  this  program;  if   not,  write  to  the  Free  Software
Foundation, Inc.,  59 Temple Place,  Suite 330, Boston,  MA 02111-1307
USA
       
Version  : 0.02
Author   : T.v.Dein <tlinden@cpan.org>
Keywords : kill delete
URL      : https://github.com/tlinden/viking-mode
License  : GNU General Public License >= 2
