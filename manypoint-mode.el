;;; manypoint-mode.el --- Keep a list of points to jump between

;; Copyright (C) 2015 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: http://github.com/andreasjansson/manypoint-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Manypoint lets you have many points in the same or different buffers.
;; It's a bit like register points, but points get updated as you move
;; around.  Useful for editing large files.
;;
;; Key bindings:
;;  * M-SPC s
;;    - save current point
;;  * M-SPC j
;;    - jump to a point
;;  * M-SPC c
;;    - clear all points

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar manypoint-mode-line " ManyP")
(defvar manypoint-points nil)
(defvar manypoint-current-point nil)
(defconst manypoint-names (concat (loop for c from 97 to 122 collect c)
                                  (loop for c from 48 to 57 collect c)))

(defface manypoint-hightlight-face
  '((t :background "magenta"
       :foreground "black"
       :weight 'bold))
  "Manypoint highlight in jump windows"
  :group 'manypoint-faces)

(defstruct (manypoint-p)
  name
  buffer
  pos)

(define-minor-mode manypoint-mode
  "Have many virtual cursors and jump between them easily"
  :global t
  :lighter manypoint-mode-line
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-SPC s") 'manypoint-save-current-point)
            (define-key map (kbd "M-SPC j") 'manypoint-jump)
            (define-key map (kbd "M-SPC c") 'manypoint-clear)
            map)
  (manypoint-clear))

(defun manypoint-clear ()
  "Clear all points."
  (interactive)
  (setq manypoint-points (make-hash-table))
  (manypoint-save-current-point))

(defun manypoint-save-current-point ()
  "Save current position/buffer as new point."
  (interactive)
  (let ((p (make-manypoint-p :name (manypoint-new-name) :buffer (current-buffer) :pos (point))))
    (puthash (manypoint-p-name p) p manypoint-points)
    (setq manypoint-current-point p))
  (manypoint-set-current-mode-line))

(defun manypoint-new-name ()
  (let ((name (loop for c across manypoint-names
                    unless (gethash c manypoint-points)
                    return c)))
    (unless name
      (user-error "Too many points!"))
    name))

(defun manypoint-jump ()
  "Jump to a point."
  (interactive)
  (let ((window-conf (current-window-configuration))
        (n-points (hash-table-count manypoint-points))
        (overlays))
    (when (> n-points 1)
      (setf (manypoint-p-buffer manypoint-current-point) (current-buffer))
      (setf (manypoint-p-pos manypoint-current-point) (point))
      (delete-other-windows)
      (manypoint-open-windows n-points)
      (setq manypoint-mode-line "")
      (force-mode-line-update)
      (loop for c across manypoint-names
            for p = (gethash c manypoint-points)
            unless p
            return nil
            for pos = (manypoint-p-pos p)
            do (switch-to-buffer (manypoint-p-buffer p))
            for overlay = (make-overlay pos (1+ pos))
            do (overlay-put overlay 'display (string c))
            do (overlay-put overlay 'face 'manypoint-hightlight-face)
            do (add-to-list 'overlays overlay)
            do (goto-char pos)
            do (other-window 1))
      (let* ((inhitit-quit t)
             (c (read-char "Jump to point: "))
             (p (gethash c manypoint-points)))
        (loop for overlay in overlays
              do (delete-overlay overlay))
        (set-window-configuration window-conf)
        (if p
            (progn
              (switch-to-buffer (manypoint-p-buffer p))
              (goto-char (manypoint-p-pos p))
              (setq manypoint-current-point p)
              (manypoint-set-current-mode-line))
          (when (not (eq c 7))
            (user-error (format "No point named %s" (string c)))))))))

(defun manypoint-set-current-mode-line ()
  (let* ((p manypoint-current-point)
         (name (manypoint-p-name p)))
   (setq manypoint-mode-line (format " ManyP<%s>" (string name)))))

(defun manypoint-open-windows (n-points)
  (destructuring-bind (rows cols-list) (manypoint-get-window-split n-points)
    (loop for row below rows
          for cols in cols-list
          do (progn (when (< row (1- rows))
                     (split-window-vertically))
                    (loop for col below (1- cols)
                          do (split-window-horizontally)
                          do (balance-windows))
                    (other-window cols)
                    (balance-windows)))))

(defun manypoint-get-window-split (n-points)
  (let* ((height (window-body-height))
         (width (window-body-width))
         (ratio 1)
         (rows (round (sqrt n-points)))
         (cols (ceiling (/ (float n-points) rows)))
         (cols-list (loop for r below rows
                          for sum = cols then (+ cols sum)
                          for c = (- cols (max 0 (- sum n-points)))
                          collect c)))
    (list rows cols-list)))


(print minor-mode-alist)

(provide 'manypoint-mode)
;;; manypoint-mode ends here
