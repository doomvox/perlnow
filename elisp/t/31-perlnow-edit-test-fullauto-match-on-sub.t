#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 31-perlnow-edit-test-fullauto-match-on-sub.t

;; The test story:

;; Create a module (non-oop, exporter)
;; Create a sub.
;; Do an edit-test from that sub.
;; Should create a test file named using both module and sub.
;; Back to the module buffer.
;; Navigate downwards, then create another sub.
;; Do an edit-test from the new sub.
;; Should create still another test, using mod and the current sub.
;; Back to the module buffer.
;; Navigate back to the original sub.
;; Do an edit-test.
;; Should re-open the first test file, matching on both module and sub.
;; Back to the module buffer.
;; Navigate down to the bottom, away from all subs.
;; Do an edit-test.
;; Should open the most recent test file that matches on module name.

;; Note: this test script does not kill opened buffers,
;; so here "open" means select an existing buffer.

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   (if (file-exists-p "test-init-elisp.el")
       (load-file "test-init-elisp.el"))

   ;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
   (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")
   (perlnow-tron) ;; DEBUG
   (let* ((e-test-loc (test-init)))
     ;; make sure you know the policy in use
     (setq perlnow-test-policy-test-location   "../t")
     (setq perlnow-test-policy-dot-definition  "incspot")
     (setq perlnow-test-policy-naming-style    "fullauto")

     (setq perlnow-pm-location
           (perlnow-fixdir (concat e-test-loc "et_fa" perlnow-slash "lib")))
     (perlnow-ensure-directory-exists perlnow-pm-location)

     (let*
         (
          (funcname "perlnow-edit-test-file")
          (test-name
           (concat "Testing " funcname ))
          (package-name "Rabid::Foosball")
          (expected-pm-base "Foosball.pm")
          (expected-pm-file
           (concat perlnow-pm-location "Rabid" perlnow-slash expected-pm-base))

          ;; for testing, we insert two subroutines named 'spin' and 'jerk'
          (new-sub-1 "spin")
          (new-sub-2 "jerk")

          (expected-t-loc
           (perlnow-fixdir
            (concat perlnow-pm-location perlnow-test-policy-test-location)))

          pm-file         pm-buffer         expected-pm-file
          first-t-file    first-t-buffer    expected-first-t-file
          second-t-file   second-t-buffer   expected-second-t-file
          again-first-t-file
          again-second-t-file
          )

       ;; clear the decks
       (test-init-safe-recursive-delete expected-t-loc)
       (perlnow-ensure-directory-exists expected-t-loc)

       ;; create and open new module file, add a sub
       (perlnow-module perlnow-pm-location package-name)
       (perlnow-insert-sub new-sub-1)
       (save-buffer)
       (setq pm-file (buffer-file-name))
       (setq pm-buffer (current-buffer))

       ;; Do an edit-test (note: we're inside first sub)
       (setq expected-first-t-file
             (concat expected-t-loc "01-Rabid-Foosball-spin.t"))
       (perlnow-edit-test-file)
       (setq first-t-file (buffer-file-name))

       (assert-equal expected-first-t-file first-t-file
                     (concat test-name ": create new test file using module and sub names"))

       ;; Back to the module buffer, navigate down, then create another sub.
       (set-buffer pm-buffer)
       (end-of-defun)
       (open-line 2)
       (forward-line 2)
       (perlnow-insert-sub new-sub-2)
       (save-buffer)

       ;; Do an edit-test from the new sub.
       (setq expected-second-t-file
             (concat expected-t-loc "02-Rabid-Foosball-jerk.t"))
       (perlnow-edit-test-file)
       (setq second-t-file (buffer-file-name))

       (assert-equal expected-second-t-file second-t-file
                     (concat test-name ": create another test file using module and current sub"))

       ;; Back to the module buffer, navigate back to the original sub.
       (set-buffer pm-buffer)
       (search-backward "=item spin")
       (search-forward "sub spin")
       (forward-line 2)

       ;; Do an edit-test.
       ;; Should re-open the first test file, matching on both module and sub.
       (perlnow-edit-test-file)
       (setq again-first-t-file (buffer-file-name))

       (assert-equal expected-first-t-file again-first-t-file
                     (concat test-name ": re-opened existing test file, matching on module & sub"))

       ;; Back to the module buffer, navigate down to the bottom, away from all subs.
       ;; Do an edit-test, Should open the most recent test file that matches on module name.
       (set-buffer pm-buffer)
       (goto-char (point-max))
       (forward-line -12)
       (perlnow-edit-test-file)
       (setq again-second-t-file (buffer-file-name))
       (assert-equal expected-second-t-file again-second-t-file
                     (concat test-name ": re-opened existing test file, going for most recent match on module."))
         ))
   (end-tests)
   ))



;;========
;; LICENSE

;; This program is free software; you can redistribute it and/or modify it
;; under the same terms as the version of GNU Emacs you intend to use it with.

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

;;; perlnow-test.el ends here
