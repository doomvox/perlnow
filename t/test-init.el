;;; test-init.el ---

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

;;   (if (file-exists-p "test-init.el")
;;      (load-file "test-init.el"))

;; This file is a place to put shared set-up customizations for
;; the elisp *.t files in the same directory.

(provide 'test-init)
(require 'cl-lib)

;; general purpose file-system utility
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

;; load-path additions
(setq load-path (cons
                 (test-init-fixdir "$HOME/lib/emacs/local")
                 load-path))
(setq load-path (cons
                 (test-init-fixdir "$HOME/End/Cave/Perlnow/lib/perlnow/elisp/")
                 load-path))
(setq load-path (cons
                 (test-init-fixdir "$HOME/End/Sys/Emacs/emacs-test-simple/")
                 load-path))

(defconst test-init-slash (convert-standard-filename "/")
  "A more portable form of the file system name separator.")

;; TODO why not the full test-init-* prefix on the following vars?
(defvar test-bin
  (file-name-directory
   (cond (load-file-name load-file-name)
         (t
          (concat test-init-slash "tmp")
          )))
  "Location of this test script.
Falls back to unix /tmp if not run as script.")

(defvar test-data
  (file-name-as-directory
   (concat test-bin "dat")))

(defvar test-loc
  (file-name-as-directory
   (substitute-in-file-name
    (concat "$HOME" test-init-slash
            "tmp"   test-init-slash
            "perlnow_test")))
  "Root location for perlnow tests: a tree of scratch directories.")

(defvar test-init-loc-isolated ""
  "Full path to a specific scratch directory for the current tests.")

(require 'test-simple)
(require 'template)
(template-initialize)
(require 'perlnow)

(setenv "USE_TAP" "t")

;;========
;; perlnow-specific functions

(defun test-init ()
  "Initialize a perlnow test. Wrapper around test-init-standard."
  (setq perlnow-quiet t) ;; ask me no questions
  ;; (perlnow-tron)
  (let ((loc (test-init-standard)) )
    loc))

(defun test-init-setup-perlnow-locations ( sub-directory )
  "Sets up standard perlnow locations using the given SUB-DIRECTORY.
Creates the SUB-DIRECTORY in the `test-loc' location, and adds
the tree of standard perlnow locations (bin, lib, dev, etc), setting
the perlnow customization variables to those locations.
NOTE: if SUB-DIRECTORY already exists it is deleted first
with `test-init-safe-recursive-delete'.
The goal here is isolated tests without side-effects on each
other.
Returns the full-path to the new sub-directory."
  (let* ((deep-test-loc
          (test-init-fixdir (concat test-loc sub-directory))))
    (test-init-safe-recursive-delete deep-test-loc)
    (test-init-mkpath deep-test-loc)
    (setq perlnow-script-location
          (file-name-as-directory (concat deep-test-loc "bin")))
    (setq perlnow-pm-location
          (file-name-as-directory (concat deep-test-loc "lib")))
    (setq perlnow-dev-location
          (file-name-as-directory (concat deep-test-loc "dev")))
    (setq perlnow-etc-location
          (file-name-as-directory (concat deep-test-loc "etc")))
    (setq perlnow-incspot-from-t-stash-file
          (concat perlnow-etc-location "incspot_from_t.json"))
    (test-init-mkpath perlnow-script-location)
    (test-init-mkpath perlnow-pm-location)
    (test-init-mkpath perlnow-dev-location)
    (test-init-mkpath perlnow-etc-location)
    (setq test-init-loc-isolated deep-test-loc)
    deep-test-loc))

(defun test-init-standard ()
  "Generates a test tree in a sub-directory named with the script's file-name prefix.
E.g. for 02-check_it.t, creates a \"t02\" in `test-loc' by running
\\[test-init-setup-perlnow-locations].  Returns the full path to the new location."
  (let (test-loc-subdir)
    (setq test-loc-subdir (test-init-standard-core))
    (test-simple-start) ;; Zero counters and start the stop watch.
    test-loc-subdir))

(defun test-init-standard-core ()
  "Implements main work done by \\[test-init-standard].
You might call this directly if you want the test location
without running \\[test-simple-start], but also see `test-init-loc-isolated'."
  (let* ( count-args
          script-file-name
          file-prefix
          sub-directory
          test-loc-subdir )
    (setq count-args (length command-line-args))
    (setq sub-directory
          (cond ((>= count-args 2)
                 (setq script-file-name (nth 2 command-line-args))
                 (setq file-prefix
                     (cond ( (string-match "\\.t$" script-file-name)
                             (car (split-string (file-name-nondirectory script-file-name) "-"))
                            )
                           (t ;; if no numeric prefix, use this alternate string
                            "XX"
                           )))
                 ;; (setq script-loc (file-name-directory script-file-name))
                 (concat "t" file-prefix)
                )
                (t
                 "tXXXX"
                 )))
    (setq test-loc-subdir (test-init-setup-perlnow-locations sub-directory))
    ;; (test-simple-start) ;; Zero counters and start the stop watch.
    test-loc-subdir))


;;========
;; General purpose file-system utilities
;; (TODO move to another *.el package some day?)
;; Note: some others are defined up top so they can be used in defvar's
;;
(defun test-init-mkpath (dir)
  "Create directory DIR (and intervening levels) if it doesn't exist."
  (unless (file-directory-p dir)
     (make-directory dir t)))

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

;; TODO could you replace this with:
;;   (setq trash-directory "Old")
;;   (move-file-to-trash dirname)
(defun test-init-safe-recursive-delete (dirname &optional backup-location safety-override)
  "Given a DIRNAME including a full-path, move it to the BACKUP-LOCATION.
BACKUP-LOCATION defaults to a sub-directory named \"Old\".
If a directory of this name already exists in the backup-location,
this will delete it first: we preserve only the last version.
As a safety feature, this first checks to make sure that the DIRNAME
contains a word such as 'tmp', 'temp' or 'test', indicating that
it's intended to be ephemeral.  If SAFETY-OVERRIDE is non-nil, this check is skipped"
  (setq dirname (test-init-fixdir dirname))
  (cond ((file-exists-p dirname)
         (let* (
                ;; remove trailing slash so file-name-directory & nondirectory can work
                (last-slash-pat (concat test-init-slash "$"))
                (dirname-trimmed
                 (replace-regexp-in-string last-slash-pat "" dirname))
                (dirname-path      (file-name-directory    dirname-trimmed))
                (dirname-sans-path (file-name-nondirectory dirname-trimmed))

                dirname-path-fixed default-backup-location
                new-backup new-backup-temp )

           ;; bring back trailing slash
           (setq dirname-path-fixed (test-init-fixdir dirname-path))
           (setq default-backup-location  (test-init-fixdir (concat dirname-path-fixed "Old")))

           (unless backup-location
             (setq backup-location default-backup-location))
           (test-init-mkpath backup-location)

           (setq new-backup
                 (test-init-fixdir
                  (concat backup-location dirname-sans-path)))

           ;; Get a uniq directory name to use temporarily
           (setq new-backup-temp new-backup)
           (let ((suffix "A")
                 (count   0))
             (while (file-exists-p new-backup-temp)
               (setq new-backup-temp
                     (concat backup-location (concat suffix (number-to-string count))))
               (setq count (1+ count)) ))

           (cond ((or
                   safety-override
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

;; TODO
;; Might be able to just use this: dired-copy-file-recursive
;; but it is has no documentation.
(defun test-init-copy-tree (this there)
  "Copies the tree of files THIS over to location THERE.
Shells out to the tar command.  If it it looks like THIS is in
THERE aready, does a \\[test-init-safe-recursive-delete] on it
first."
  (unless (file-accessible-directory-p this)
    "The first argument must be an existing directory.")
  (unless (file-accessible-directory-p there)
    "The second argument must be an existing directory.") ;; TODO Letting me run with non-existant there dir?
  (setq this  (test-init-fixdir this))
  (setq there (test-init-fixdir there))
  (let* (
         ;; remove trailing slash so file-name-directory & nondirectory can work
         (tail-slash-pat (concat perlnow-slash "$"))
         (this-trimmed
              (replace-regexp-in-string tail-slash-pat "" this))
         (orig-loc  (file-name-directory this-trimmed))
         (this-name (file-name-nondirectory this-trimmed))
         (basename  (file-name-sans-extension this-name))
         (new-tree (concat there this-name))
         (tar-file (concat basename ".tgz"))
         (tar-program  "tar")
         (tar-args (list "czf" tar-file "."))
         (display-buffname "*test-init*")
         (display-buffer (get-buffer-create display-buffname))
         (save-loc default-directory)
         )
    (if (file-accessible-directory-p new-tree)
        (test-init-safe-recursive-delete new-tree nil t))
    (cd this)
     ;; tar-cmd-1
     (call-process tar-program nil nil display-buffer
                   "czf" tar-file ".")
    (test-init-mkpath there)
    (copy-file tar-file there t) ;; over-write is okay
    (move-file-to-trash tar-file) ;; EXPERIMENTAL

    ;; tar-cmd-2
    (setq save-loc default-directory)
    (cd there)
    (call-process tar-program nil nil display-buffer "xzf" tar-file)
    (cd save-loc)
    ))

(defun test-init-subdirs (loc)
  "A simple listing of sub-directories in the given directory LOC.
Skips hidden directories (names with leading \".\").
Restricts the listing to accessible directories.
Does not include the path.  Returns a sorted list."
  (dolist (dir (directory-files test-set nil "^[^.]"))
    (if (and
         (file-accessible-directory-p dir)
         (not (string-equal dir "meta")))
        (push dir cases)))
  cases)

;; initialize the 'perlnow_test/tNN' location from 'code/sNN':
;; e.g.
;;   /home/doom/End/Cave/Perlnow/lib/perlnow/t/dat/code/s65/*
;; ==>
;;   /home/doom/tmp/perlnow_test/t65
;;
;; full path to "dat" should be in var: test-data
(defun test-init-load-setup-code (dir-basename &optional deep-test-loc)
  "Copies initial version of directory named DIR-BASENAME to test location.
Moves any existing copy of it out of the way.  The test location
DEEP-TEST-LOC defaults to the return from \\[test-init-standard-core],
passed via `test-init-loc-isolated'."
  (unless deep-test-loc (setq deep-test-loc test-init-loc-isolated))
  ;; (message "test-init-loc-isolated: %s" test-init-loc-isolated) ;; /home/doom/tmp/perlnow_test/t65/
  (let* ((slash test-init-slash)
         (data-loc   (test-init-fixdir (concat default-directory "dat" slash)))
         (source-loc (concat data-loc "code" slash dir-basename slash))
;;         (source-loc default-directory)
         )
  ; (message "source-loc: %s" source-loc) ;; /home/doom/End/Cave/Perlnow/lib/perlnow/t/dat/code/s65/

  (test-init-copy-tree source-loc deep-test-loc)
  ))


(defun test-init-load-data (file)
  "Re-load data structure from elisp FILE generated by a pp dump."
  (let ( raw-string  data )
    (unless (file-exists-p file)
      (message "test-init-load-data: file does not exist: %s" file))
    (find-file file)
    (setq raw-string (buffer-substring-no-properties (point-min) (point-max)))
    (setq data (read raw-string))
    (kill-buffer)
    data))

(defun test-init-preserve-data (data file)
  "Save given elisp DATA to the FILE, using a pp dump."
  (let ( raw-string )
   (find-file file)
    ;; TODO what safety feature can I put on this?
    (delete-region (point-min) (point-max))
    (insert (pp-to-string data))
    (basic-save-buffer)
    ))

;; simpler vesion of perlnow-stash-lookup, but evals the val
(defun test-init-plist-lookup (keystr plist-symbol)
  "Look-up string KEYSTR in the plist indicated by PLIST-SYMBOL.
If KEYSTR is nil, returns nil." ;; TODO not sure it matters if KEYSTR is a string
  (let ((ret
         (cond (keystr
                (let* ((raw (lax-plist-get (symbol-value plist-symbol) keystr))
                       (value (eval raw))
                      )
                  value))
               (t
                nil))))
    ret))






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
