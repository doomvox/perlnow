;;; test-init-elisp.el ---

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

;;; Commentary:

;; This is essentially an include file, to be called from an
;; elisp *.t file like so:

;;   (if (file-exists-p "test-init-elisp.el")
;;      (load-file "test-init-elisp.el"))

;; This file is a place to put shared set-up customizations for
;; the elisp *.t files in the same directory.

(provide 'test-init-elisp)
(require 'cl-lib)

;; load-path additions
(setq load-path (cons "/home/doom/lib/emacs/local" load-path))
(setq load-path (cons "/home/doom/End/Cave/Perlnow/lib/perlnow/elisp/" load-path))

;; (message "LP, i-level 2: %s" (pp load-path))
;; (princ "LP, i-level 1: %s" (pp load-path))

(defvar test-bin
  (file-name-directory load-file-name)
  "Location of this test script.")

(defvar test-loc
  (file-name-as-directory
   (substitute-in-file-name "$HOME/tmp/perlnow_test"))
  "Root location for perlnow tests.")

;; (setq test-loc
;;   (file-name-as-directory
;;    (substitute-in-file-name "$HOME/tmp/perlnow_test")))

(require 'template)
(template-initialize)
(require 'perlnow)

(setenv "USE_TAP" "t")

;; I always want one of these:
(defun test-init-fixdir (dir &optional root)
  "Fixes the DIR.
Conditions directory paths for portability and robustness.
Some examples:
 '~/tmp'             => '/home/doom/tmp/'
 '~/tmp/../bin/test' => '/home/doom/bin/test/'
Note: converts relative paths to absolute, using the current
default-directory setting, unless specified otherwise with the
ROOT option.  Note side-effect: converts the empty string into
the default-directory or the ROOT setting."
  (let ((return
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name dir root))))))
    return))

(defun test-init-mkpath (dir)
  "Create directory DIR (and intervening levels) if it doesn't exist."
  (unless (file-directory-p dir)
     (make-directory dir t)))

(defun test-init ()
  "Generates a test tree in a sub-directory named with the script's file-name prefix.
E.g. for 02-check_it.t, creates a \"t02\" in `test-loc' by running
\\[test-init-setup-perlnow-locations]."
  (let* (
         (script-file-name (nth 2 command-line-args))
         (file-prefix (car (split-string (file-name-nondirectory script-file-name) "-")))
         (script-loc (file-name-directory script-file-name))
         (sub-directory (concat "t" file-prefix))
         (test-loc-subdir (test-init-setup-perlnow-locations sub-directory))
         )
    (setq perlnow-force t) ;; ask me no questions
    (test-simple-start) ;; Zero counters and start the stop watch.
    test-loc-subdir))

(defun test-init-setup-perlnow-locations ( sub-directory )
  "Sets up standard perlnow locations using the given SUB-DIRECTORY.
Creates the SUB-DIRECTORY in the `test-loc' location, and adds
the tree of standard perlnow locations (bin, lib, dev), setting
the perlnow customization variables to those locations.
NOTE: if SUB-DIRECTORY already exists it is deleted first
with `test-init-safe-recursive-delete'.
The goal here is isolated tests without side-effects on each
other.
Returns the full-path to the new sub-directory."
  (let* ((deep-test-loc
          (perlnow-fixdir (concat test-loc sub-directory)))
         )
    (test-init-safe-recursive-delete deep-test-loc)

    (perlnow-mkpath deep-test-loc)

    (setq perlnow-script-location
          (file-name-as-directory (concat deep-test-loc "bin")))
    (setq perlnow-pm-location
          (file-name-as-directory (concat deep-test-loc "lib")))
    (setq perlnow-dev-location
          (file-name-as-directory (concat deep-test-loc "dev")))

    (perlnow-mkpath perlnow-script-location)
    (perlnow-mkpath perlnow-pm-location)
    (perlnow-mkpath perlnow-dev-location)
    deep-test-loc))

;; TODO move all of the following defuns to a new package:
;;    test-simple-script.el
(defun test-init-move-file-out-of-way (filename &optional extension)
  "Move FILENAME out of the way, by renaming it with appended EXTENSION.
Default EXTENSION is \".OLD\""
  (let* ((extension (cond (extension extension)
                          (t ".OLD")))
         (backup-name (concat filename extension))
         )
   (cond ((file-exists-p filename)
          (message "renaming existing file: %s as %s" filename backup-name)
          (rename-file filename backup-name t) ;; with t, overwrites
          ))
   ))

(defun test-init-safe-recursive-delete (dirname &optional backup-location)
  "Given a DIRNAME including a full-path, move it to the BACKUP-LOCATION.
BACKUP-LOCATION defaults to a sub-directory named \"Old\".
If a directory of this name already exists in the backup-location,
this will delete it first: we preserve only the last version.
As a safety feature, this first checks to make sure that the DIRNAME
contains a word such as 'tmp', 'temp' or 'test', indicating that
it's intended to be ephemeral."
  (setq dirname (test-init-fixdir dirname))
  (cond ((file-exists-p dirname)
         (let* (
                ;; drop trailing slash for file-name-directory & nondirectory
                (last-slash-pat (concat perlnow-slash "$"))
                (dirname-trimmed
                 (replace-regexp-in-string last-slash-pat "" dirname))
                (dirname-path      (file-name-directory    dirname-trimmed))
                (dirname-sans-path (file-name-nondirectory dirname-trimmed))

                ;; bring back trailing slash
                (dirname-path-fixed (test-init-fixdir dirname-path))

                (default-backup-location  (concat dirname-path-fixed "Old"))
                new-backup new-backup-temp )
           (unless backup-location
             (setq backup-location default-backup-location))
           (test-init-mkpath backup-location)

           (setq new-backup
                 (concat backup-location dirname-sans-path))

           ;; Get a uniq directory name to use temporarily
           (setq new-backup-temp new-backup)
           (let ((suffix "A")
                 (count   0))
             (while (file-exists-p new-backup-temp)
               (setq new-backup-temp (concat new-backup (concat suffix (number-to-string count))))
               (setq count (1+ count)) ))

           (cond ((or
                   (string-match "\\btest\\b" dirname)
                   (string-match "\\btmp\\b"  dirname)
                   (string-match "\\btemp\\b" dirname)
                   )
                  ;; first copy to a dir with unique name, then
                  ;; delete orignal, and rename the copy using
                  ;; original name
                  (copy-directory dirname new-backup-temp nil t t)
                  (delete-directory dirname t)
                  (if (file-directory-p new-backup)
                      (delete-directory new-backup t))
                  (copy-directory new-backup-temp new-backup nil t t)
                  ))))))


;; LICENSE

;; This program is free software; you can redistribute it and/or modify
;; it under the same terms as the version of GNU Emacs you intend to use it with.

;; At present, GNU Emacs is under the GNU General Public License version 3
;; or (at your option) any later version.  This license is as published by
;; the Free Software Foundation.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; elisp-test-init.el ends here
