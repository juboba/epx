;;; epx.el --- Emacs Project eXecutor: Manage and run project-specific shell commands -*- lexical-binding: t -*-

;; Copyright (c) 2025 Oleksandr Korzh

;; Author: Oleksandr Korzh <alex@korzh.me>
;; Version 0.1
;; Package-Requires: ((emacs "29.1"))
;; URL: TODO
;; Keywords: project, shell, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; epx is a command runner and manager for project.el in Emacs.
;; It stores commands in .dir-locals.el or any other dir-locals-file that
;; you set. It allows you to add or remove commands, no editing
;; capabilities for now. You can choose whether to use compilation
;; buffer when you create your command.

;;; Code:
(require 'project)
(require 'cl-lib)
(require 'files)
(require 'files-x)

;; TODO think of better env parsing/entering
;; TODO consider other storages again (separate .el, .json, .toml...)
;; TODO Package Headers


(defun epx--current-project-root ()
  "Return current project’s root.  If there’s no project, throw an error."
  (if (project-current)
      (project-root (project-current))
    (error "No project found.  This only works in projects")))


(defun epx--locals-file ()
  "Return the path to the current project's dir-locals-file."
    (expand-file-name dir-locals-file (epx--current-project-root)))


(defun epx--create-locals-file ()
  "Create dir-locals-file in current project root.
If the file already exists, do nothing."
  (let ((file (epx--locals-file)))
    (unless (file-exists-p file)
      (write-region "" nil file))))


(defun epx--read-shell-command ()
  "Prompt for a shell command with completion from dir-locals-file'."
  (let* ((locals-file (epx--locals-file))
         (cmds (if (file-exists-p locals-file)
                   (epx--read-cmds-locals)))
         (history (mapcar (lambda (plist) (plist-get plist :name)) cmds))
         (name (completing-read "Project command: " history nil t)))
    (cl-find-if (lambda (plist) (string= (plist-get plist :name) name)) cmds)))


(defun epx--get-or-create-shell-window (project-root)
  "Return a window with a shell for PROJECT-ROOT, creating one if necessary."
  (or (cl-find-if
       (lambda (win)
         (with-current-buffer (window-buffer win)
           (and (eq major-mode 'shell-mode)
                (string-prefix-p project-root default-directory))))
       (window-list nil 'nomini))
      (progn
        (split-window-sensibly)
        (other-window 1)
        (project-shell)
        (selected-window))))


;;;###autoload
(defun epx-run-command-in-shell (&optional command)
  "Run COMMAND in a PROJECT shell buffer.
If a shell window already exists, reuse it.  Otherwise open one.
When called interactively, prompt for COMMAND with completion from history."
  (interactive
   (list (epx--read-shell-command)))
  (let* ((root (epx--current-project-root))
        
	 (env-list (plist-get command :env))
	 (cmd (if env-list
		  (concat (epx--prepare-env env-list) " " (plist-get command :command))
		(plist-get command :command)))
	 (use-compilation (plist-get command :compile)))
    (if use-compilation
	(let ((default-directory root))
	  (compilation-start cmd nil )) ;; TODO:  compilation-buffer-name-function - project-local buffer name
      (let ((win (epx--get-or-create-shell-window root)))
	(select-window win)
	;; Insert command into shell buffer
	(run-with-timer
	 0.1 nil
	 (lambda ()
	   (goto-char (point-max))
	   (insert cmd)
	   (comint-send-input)))))))


;;;###autoload
(defun epx-remove-command (&optional command)
  "Delete COMMAND from dir-locals-file."
  (interactive
   (list (epx--read-shell-command)))
  (if (y-or-n-p (format "Are you sure you want to remove command %s?" (plist-get command :name)))
      (let* ((locals-file (epx--locals-file))
	     (local-project-cmds (epx--read-cmds-locals) )
             (updated (cl-remove command local-project-cmds :test #'equal)))
	(with-current-buffer (find-file-noselect locals-file)
	  (delete-dir-local-variable nil 'local-project-cmds)
	  (add-dir-local-variable nil 'local-project-cmds updated)
	  (save-buffer)
	  (kill-buffer)))))


(defun epx--parse-env (env)
  "Parse ENV from a string separated by semicolons into a list of plists."
  (mapcar (lambda(s)
	    (let ((pair (split-string s "\=")))
	      (list :name (car pair) :value (cadr pair))))
	  (split-string env "\\;")))


(defun epx--prepare-env (env-list)
  "Convert ENV-LIST from the list of plists into a semicolon-separated string."
  (string-join
   (mapcar (lambda (el)
	     (concat (plist-get el :name) "=" (plist-get el :value)))
	   env-list)
   ";"))


;;;###autoload
(defun epx-add-command (&optional cmd name env compile)
  "Add a new command to dir-locals-file interactively.
CMD and NAME are expected to be non-empty.
ENV and COMPILE default to nil."
  (interactive
   (let* ((_ (epx--current-project-root)) ;; to check we’re in the project
	  (cmd (read-string "Shell command to run: "))
          (_ (when (string-empty-p cmd)
               (user-error "Command cannot be empty")))
          (name (read-string "Command name: "))
          (_ (when (string-empty-p name)
               (user-error "Command name cannot be empty")))
          (env (read-string "Env vars (semicolon-separated): "))
          (compile (y-or-n-p "Do you want to use compilation buffer for your command?")))
     (list cmd name env compile)))
  (epx--create-locals-file)
  (let ((new-cmd (list :name name :command cmd :env (if (equal "" env)
							nil
						      (epx--parse-env env))
		       :compile compile)))
    (epx--record-command new-cmd)))


(defun epx--read-cmds-locals ()
  "Read project commands from dir-locals-file"
  (hack-dir-local-variables)
  (alist-get 'local-project-cmds file-local-variables-alist nil nil #'equal))


(defun epx--record-command (command)
  "Add COMMAND to `local-project-cmds' in dir-locals-file.
If a command with the same name already exists, throw an error"
  (let ((locals-file (epx--locals-file)))
    (when (file-exists-p locals-file)
      (let* ((existing-cmds (epx--read-cmds-locals))
             (duplicate (cl-find-if (lambda (cmd)
                                      (string= (plist-get cmd :name)
                                               (plist-get command :name)))
                                    existing-cmds)))
        (if duplicate
            (error "A command with the name '%s' already exists" (plist-get command :name))
          (let ((new-cmds (cons command existing-cmds)))
            (with-current-buffer (find-file-noselect locals-file)
              (add-dir-local-variable nil 'local-project-cmds new-cmds)
              (save-buffer)
              (kill-buffer))))))))

(provide 'epx)

;;; epx.el ends here
