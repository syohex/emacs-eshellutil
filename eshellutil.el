;;; eshellutil.el --- My own Eshell utilities

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-eshellutil
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:

(require 'eshell)
(require 'em-dirs)
(require 'vc-git)

(declare-function elscreen-get-current-screen "elscreen")
(declare-function magit-status "magit")

(defgroup eshellutil nil
  "eshell utilities"
  :group 'eshell)

(defface eshellutil-prompt-directory
  '((t (:foreground "green" :weight bold)))
  "Face of directory in eshell prompt."
  :group 'eshellutil)

(defface eshellutil-prompt-git-branch
  '((t (:foreground "magenta" :weight bold)))
  "Face of 'git branch' in eshell prompt"
  :group 'eshellutil)

(defun eshellutil--git-dirty ()
  (let ((ret (process-file "git" nil nil nil "diff-index" "--quiet" "HEAD")))
    (if (= ret 1)
        "*"
      "")))

(defun eshellutil--prompt-branch ()
  (let ((cur-branch (car (vc-git-branches))))
    (if (not cur-branch)
        ""
      (concat "(" cur-branch (eshellutil--git-dirty) ") "))))

(defun eshellutil--prompt-cwd ()
  (let ((cwd (abbreviate-file-name (eshell/pwd))))
    (with-temp-buffer
      (insert cwd)
      (let ((depth (count-matches "/" (point-min) (point-max))))
        (if (<= depth 3)
            cwd
          (goto-char (point-min))
          (search-forward "/" nil t 2)
          (let ((parent (buffer-substring-no-properties (point-min) (point))))
            (search-forward "/" nil t (- depth 2 1))
            (let ((child (buffer-substring-no-properties (1- (point)) (point-max))))
              (concat parent ".." child))))))))

(defun eshellutil-prompt ()
  (format "[%s] %s%s "
          (propertize (eshellutil--prompt-cwd) 'face 'eshellutil-prompt-directory)
          (propertize (eshellutil--prompt-branch) 'face 'eshellutil-prompt-git-branch)
          (propertize "%" 'face 'bold)))

(defsubst eshellutil--window-register-name ()
  (if (not (featurep 'elscreen))
      :eshellutil
    (intern (format "eshellutil%d" (elscreen-get-current-screen)))))

(defvar eshellutil--previous-buffer nil)

(defun eshellutil--save-current-windows ()
  (setq eshellutil--previous-buffer (current-buffer))
  (let ((register (eshellutil--window-register-name)))
    (window-configuration-to-register register)))

(defsubst eshellutil--shell-buffer-name ()
  (if (featurep 'elscreen)
      (format " *eshellutil<%d>*" (elscreen-get-current-screen))
    " *eshellutil*"))

(defun eshellutil--validate-window-number ()
  (let ((windows (length (window-list))))
    (when (>= windows 3)
      (error "Error: too many windows(%d)." windows))))

(defsubst eshellutil--buffer-visible-p (bufname)
  (cl-loop for win in (window-list)
           for winbuf = (window-buffer win)
           thereis (string= bufname (buffer-name winbuf))))

;;;###autoload
(defun eshellutil-popup ()
  (interactive)
  (let ((shell-bufname (eshellutil--shell-buffer-name)))
    (if (eshellutil--buffer-visible-p shell-bufname)
        (progn
          (other-window 1))
      (eshellutil--save-current-windows)
      (when (>= (length (window-list)) 3)
        (delete-other-windows))
      (when (one-window-p)
        (split-window-right))
      (other-window 1)
      (let ((shell-buf (get-buffer shell-bufname)))
        (if (buffer-live-p shell-buf)
            (progn
              (switch-to-buffer shell-buf)
              (goto-char (point-max))
              (eshell-kill-input))
          (eshell)
          (rename-buffer shell-bufname))))))

(defun eshellutil-restore ()
  (interactive)
  (when (and (eq major-mode 'eshell-mode)
             (not (string= (buffer-name) (eshellutil--shell-buffer-name))))
    (error "This buffer is *eshell*, but this is not popup-ed buffer."))
  (let ((register (eshellutil--window-register-name)))
    (jump-to-register register)))

(defun eshellutil-recenter ()
  (interactive)
  (recenter 0))

(defun eshellutil-kill-output ()
  (interactive)
  (let* ((kill-func (if current-prefix-arg 'kill-region 'delete-region)))
    (save-excursion
      (goto-char (eshell-beginning-of-output))
      (insert "*** output flushed ***\n")
      (let ((current-prefix-arg nil))
        (funcall kill-func (point) (eshell-end-of-output))))))

;;
;; commands
;;

(defun eshell--cde-last-directory ()
  (let ((file-name (buffer-file-name eshellutil--previous-buffer)))
    (or (and file-name (file-name-directory file-name))
        (and (eq major-mode 'dired-mode) dired-directory)
        (with-current-buffer eshellutil--previous-buffer
          default-directory))))

(defun eshell/cde ()
  (let ((dir (eshell--cde-last-directory)))
    (eshell/cd dir)))

(defun eshell/cdp ()
  (let ((rootdir (or (and (fboundp 'vc-root-dir)
                          (ignore-errors (vc-root-dir)))
                     (vc-git-root default-directory))))
    (unless rootdir
      (error "Here is not managed by VCS."))
    (eshell/cd rootdir)))

(defun eshell/e (file)
  (let ((path (concat default-directory file)))
    (eshellutil-restore)
    (find-file path)))

(defun eshell/m ()
  (magit-status default-directory))

;;
;; Setup
;;

;;;###autoload
(defun eshellutil-eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "C-c C-o") 'eshellutil-kill-output)
  (define-key eshell-mode-map (kbd "C-\\") 'eshellutil-restore)
  (define-key eshell-mode-map (kbd "C-l") 'eshellutil-recenter)
  (define-key eshell-mode-map (kbd "M-g M-f") 'ffap-other-window))

(with-eval-after-load 'em-prompt
  (setq-default eshell-prompt-regexp "^[^#%\n]*[#%] ")
  (setq-default eshell-prompt-function 'eshellutil-prompt))

;;;###autoload
(add-hook 'eshell-mode-hook 'eshellutil-eshell-mode-hook)

;; aliases
(require 'em-alias)
(add-to-list 'eshell-command-aliases-list (list "s" "git st"))
(add-to-list 'eshell-command-aliases-list (list "d" "git --no-pager diff"))

(provide 'eshellutil)

;;; eshellutil.el ends here
