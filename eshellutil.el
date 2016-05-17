;;; eshellutil.el --- My own Eshell utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-eshellutil
;; Version: 0.01
;; Package-Requires: ((emacs "24.4"))

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

;;; Code:

(require 'eshell)
(require 'em-dirs)
(require 'vc-git)
(require 'em-tramp)
(require 'subr-x)

(defgroup eshellutil nil
  "eshell utilities"
  :group 'eshell)

(defface eshellutil-prompt-directory
  '((t (:foreground "yellow" :weight bold)))
  "Face of directory in eshell prompt."
  :group 'eshellutil)

(defface eshellutil-prompt-git-branch
  '((t (:foreground "magenta" :weight bold)))
  "Face of 'git branch' in eshell prompt"
  :group 'eshellutil)

(defun eshellutil--prompt-branch ()
  (let ((cur-branch (car (vc-git-branches))))
    (if (not cur-branch)
        ""
      (concat "(" cur-branch ") "))))

(defun eshellutil--prompt-cwd ()
  (let* ((cwd (eshell/pwd))
         (dir (if (string= (abbreviate-file-name cwd) "~")
                  "~"
                (file-name-nondirectory (directory-file-name cwd)))))
    (concat "[" dir "]")))

(defun eshellutil-prompt ()
  (format "%s %s%s "
          (propertize (eshellutil--prompt-cwd) 'face 'eshellutil-prompt-directory)
          (propertize (eshellutil--prompt-branch) 'face 'eshellutil-prompt-git-branch)
          (propertize "%" 'face 'bold)))

(defvar eshellutil--previous-buffer nil)

(defun eshellutil--save-current-windows ()
  (setq eshellutil--previous-buffer (current-buffer))
  (window-configuration-to-register :eshellutil))

(defconst eshellutil--shell-buffer-name " *eshellutil*")

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
  (let ((shell-bufname eshellutil--shell-buffer-name))
    (if (eshellutil--buffer-visible-p shell-bufname)
        (other-window 1)
      (eshellutil--save-current-windows)
      (when (>= (length (window-list)) 3)
        (delete-other-windows))
      (when (one-window-p)
        (if (> (window-width) 120)
            (split-window-right)
          (split-window-below)))
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
             (not (string= (buffer-name) eshellutil--shell-buffer-name)))
    (error "This buffer is *eshell*, but this is not popup-ed buffer."))
  (jump-to-register :eshellutil))

(defun eshellutil-recenter ()
  (interactive)
  (recenter 0))

(defun eshellutil-kill-output ()
  (interactive)
  (save-excursion
    (goto-char (eshell-beginning-of-output))
    (insert "*** output copied ***\n")
    (kill-region (point) (eshell-end-of-output))))

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
  (let ((rootdir (cl-loop for dir in '(".git/" ".hg/" ".svn/" "project.clj"
                                       "Gemfile" "package.json" "Build.PL" "Makefile")
                          when (locate-dominating-file default-directory dir)
                          return it)))
    (unless rootdir
      (error "Can't find project root."))
    (eshell/cd rootdir)))

(defun eshellutil--git-common (subcmd)
  (let ((str (with-temp-buffer
               (if (null args)
                   (process-file "git" nil t nil subcmd "-sb")
                 (apply #'process-file "git" nil t nil subcmd "-sb" args))
               (buffer-substring-no-properties (point-min) (point-max)))))
    (eshell-buffered-print str)
    (eshell-flush)))

(defun eshell/s (&rest args)
  (eshellutil--git-common "status"))

(defun eshell/d (&rest args)
  (eshellutil--git-common "diff"))

;;
;; Setup
;;

;;;###autoload
(defun eshellutil-eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "M-o") 'eshellutil-kill-output)
  (define-key eshell-mode-map (kbd "C-\\") 'eshellutil-restore)
  (define-key eshell-mode-map (kbd "C-l") 'eshellutil-recenter)
  (define-key eshell-mode-map (kbd "M-g M-f") 'ffap-other-window))

(with-eval-after-load 'em-prompt
  (setq-default eshell-prompt-regexp "^[^#%\n]*[#%] ")
  (setq-default eshell-prompt-function 'eshellutil-prompt))

;;;###autoload
(add-hook 'eshell-mode-hook 'eshellutil-eshell-mode-hook)

;; environment variables
(setenv "GIT_PAGER" "cat")

(provide 'eshellutil)

;;; eshellutil.el ends here
