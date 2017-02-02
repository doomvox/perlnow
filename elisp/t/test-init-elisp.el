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

(setq perlnow-script-location
      (file-name-as-directory (concat test-loc "bin")))
(setq perlnow-pm-location
      (file-name-as-directory (concat test-loc "lib")))
(setq perlnow-dev-location
      (file-name-as-directory (concat test-loc "dev")))

;;; create all unless they exist already
(perlnow-mkpath perlnow-script-location)
(perlnow-mkpath perlnow-pm-location)
(perlnow-mkpath perlnow-dev-location)

;; TODO move this somewhere: test-simple-utils.el ?
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
