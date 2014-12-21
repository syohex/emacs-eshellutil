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

(defgroup eshellutil nil
  "eshell utilities"
  :group 'eshell)

(defface eshellutil-prompt-directory
  '((t (:foreground "cyan" :weight bold)))
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
      (concat "(" cur-branch (eshellutil--git-dirty) ")"))))

(defun eshellutil-prompt ()
  (format "%s%s %s "
          (propertize (concat (file-name-nondirectory (eshell/pwd)) ":")
                      'face 'eshellutil-prompt-directory)
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

(defsubst eshellutil--eshell-index ()
  (if (featurep 'elscreen)
      (elscreen-get-current-screen)
    0))

;;;###autoload
(defun eshellutil-popup ()
  (interactive)
  (eshellutil--save-current-windows)
  (when (one-window-p)
    (split-window-right))
  (other-window 1)
  (eshell (eshellutil--eshell-index)))

(defun eshellutil-restore ()
  (interactive)
  (let ((register (eshellutil--window-register-name)))
    (jump-to-register register)))

(defun eshellutil-recenter ()
  (interactive)
  (recenter 0))

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
  (let ((dir (cl-loop with cwd = default-directory
                      for d in '(".git" ".hg" ".svn")
                      when (locate-dominating-file cwd d)
                      return (file-name-directory it))))
    (eshell/cd dir)))

(defun eshell/e (file)
  (let ((curwin (get-buffer-window))
        (filepath (concat default-directory file)))
    (other-window 1)
    (find-file filepath)
    (delete-window curwin)))

(provide 'eshellutil)

;;; eshellutil.el ends here
