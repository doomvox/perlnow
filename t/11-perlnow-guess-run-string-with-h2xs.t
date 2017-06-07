#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 11-perlnow-guess-run-string-with-h2xs.t

;; Test story:

;; The overall idea is to create a bunch of different kinds of
;; perl files in different structures, then check the result of
;; perlnow-guess-run-string in all of them.

;; Three contexts:
;;   cpan-style
;;     perlnow-module-starter
;;     perlnow-h2xs
;;   standalone:
;;     perlnow-module or perlnow-object-module
;;
;; Complication: *history* matters.
;; The run-string of a pm reflects what was just created.

;; The story in more detail:
;;    perlnow-module-starter
;;    pm-file  (from current-buffer)
;;    perlnow-back-to-code
;;    t-file
;;    back to pm
;;    perlnow-script
;;    guess run-string


;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))

          (funcname "perlnow-guess-run-string")
          (test-name (concat "Testing " funcname ))
          (mod-style   "h2xs") ;; noncpan, modstar, h2xs...
          (test-ext-from  "") ;; pl, pm, or t
          (test-ext-new   "") ;; pl, pm, or t
          (test-mess      "") ;; for "new", from "from", in "context"
          (perl "/usr/bin/perl")
          ;; h2xs (with ExtUtils::MakeMaker)
          (package-name "Hack::Excess")
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))

          ;; pm: /home/doom/tmp/perlnow_test/dev/Hack-Excess/lib/Hack/Excess.pm
          (exp-pm-base "Excess.pm")
          (exp-pm-file
           (concat staging-area "lib"
                   perlnow-slash "Hack" perlnow-slash exp-pm-base))

          ;; pl: /home/doom/tmp/perlnow_test/dev/Hack-Excess/bin/overthrust.pl
          (script-name "overthrust.pl")
          (exp-script (concat staging-area "bin" perlnow-slash script-name))

          ;; t: /home/doom/tmp/perlnow_test/dev/Hack-Excess/t/01-Hack-Excess.t
          ;;           (t-name "01-Hack-Excess.t") ;;
          (t-name "Hack-Excess.t") ;;

          (exp-t (concat staging-area "t" perlnow-slash t-name))

          pm-file          pm-buffer
          t-file           t-buffer
          script-file      script-buffer

          rs-from-script   exp-rs-from-pm
          rs-from-t        exp-rs-from-t
          rs-from-pm       exp-rs-from-pm
          )
     ;; "H2XS
     (if perlnow-debug (message (upcase mod-style)))

     (test-init-safe-recursive-delete staging-area)
     ;; For a h2xs based project (with EU::MM)
     ;; get file names and buffers, first module and test file
     ;; (along the way, do basic checks: verifying files were created as expected)
     (perlnow-h2xs perlnow-dev-location package-name)  ;; TODO DEBUG ISSUES (?)

     (setq pm-file   (buffer-file-name))
     (setq pm-buffer (current-buffer))

     (assert-t
      (perlnow-module-code-p)
      (concat "Testing that " mod-style " left new pm buffer active.")) ;; ok 1

     ;;       (perlnow-back-to-code)
     ;;       (setq t-file   (buffer-file-name))
     ;;       (setq t-buffer (current-buffer))

     (perlnow-back-to-code) ;; switch to associated *.t file
     (setq t-file   (buffer-file-name))
     (setq t-buffer (current-buffer))

     (if perlnow-debug (message "t created by " mod-style " run: %s expected: %s" t-file exp-t))
     (assert-equal exp-t t-file
                   (concat "Testing " mod-style " run created test file: \n"
                           "        " t-file )) ;; ok 2

     ;; now, a script generated from the module
     ;; H2XS: script from pm
     (setq test-ext-from  "pm") ;; pl, pm, or t
     (setq test-ext-new   "pl") ;; pl, pm, or t
     (setq test-mess (concat test-ext-new " from " test-ext-from " in " mod-style))

     (if perlnow-debug (message "YYY1: %s" test-mess))

     ;; back to the pm
     (perlnow-back-to-code)
     ;; Note, staging-area: /home/doom/tmp/perlnow_test/dev/Planet-Ten
     (setq exp-rs-from-pm ;; before we create a script, should guess the *.t
           (concat "perl " exp-t))

     (setq rs-from-pm (perlnow-guess-run-string))
     (message "REALLY rs-from-pm: %s" rs-from-pm)
     (if perlnow-debug (message "rs-from-pm pre-pl: %s" rs-from-pm))
     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm " mod-style )) ;;  ok 3

     ;; start from the pm, again, create script
     (set-buffer pm-buffer)
     ;; get rid of pre-existing script (non-interactive perlnow-script can't deal)
     (test-init-move-file-out-of-way exp-script)
     (perlnow-script exp-script)
     (setq script-file   (buffer-file-name))
     (setq script-buffer (current-buffer))
     (assert-equal exp-script script-file
                   (format "Testing perlnow-script in %s: \n %s" mod-style script-file)) ;; ok 4

     ;; back to the module again, and guess
     (set-buffer pm-buffer)

     (setq exp-rs-from-pm (concat "perl " staging-area "bin/overthrust.pl"))
     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm post-pl: %s" rs-from-pm))

     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm in " mod-style " after new pl ")) ;; ok 5

     (set-buffer script-buffer)
     (setq rs-from-script (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-script: %s" rs-from-script))

     ;; "/usr/bin/perl /home/doom/tmp/perlnow_test/dev/Planet-Ten/bin/overthrust.pl"
     (setq exp-rs-from-script (concat "/usr/bin/perl " staging-area "bin/overthrust.pl"))
     (assert-equal exp-rs-from-script rs-from-script
                   (concat "Testing " funcname " for script in " mod-style )) ;; ok 6

     ;; switch to test file, and guess
     (set-buffer t-buffer)
     (setq rs-from-t (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-t: %s" rs-from-t))

     ;; "perl /home/doom/tmp/perlnow_test/dev/Planet-Ten/t/01-Planet-Ten.t"
     ;;      (setq exp-rs-from-t (concat "perl " staging-area "t/01-Planet-Ten.t"))
     (setq exp-rs-from-t (concat "perl " exp-t))

     (assert-equal exp-rs-from-t rs-from-t
                   (concat "Testing " funcname " for t in " mod-style ))  ;; ok 7

     ;; back to pm, did guess in t change rs here?
     (set-buffer pm-buffer)
     ;; after we've done a guess in a *.t, it *doesn't* change the rs for the pm:

     ;; "perl /home/doom/tmp/perlnow_test/dev/Planet-Ten/bin/overthrust.pl"
     (setq exp-rs-from-pm
           (concat "perl " staging-area "bin/overthrust.pl"))
     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm post-pl, after t again: %s" rs-from-pm))
     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm " mod-style )) ;; not ok 8

     ;; if we switch to the pm, and do an *edit-test*, then the pm's rs changes to t
     ;; back to pm, then do an edit-test
     (set-buffer pm-buffer)
     ;; (perlnow-edit-test-file 1 t-file)
     (perlnow-open-test-file t-file) ;; faking an "edit-test" call

     (set-buffer pm-buffer)

     ;; "perl /home/doom/tmp/perlnow_test/dev/Planet-Ten/t/01-Planet-Ten.t"
     ;;       (setq exp-rs-from-pm
     ;;             (concat "perl " staging-area "t/01-Planet-Ten.t"))
     (setq exp-rs-from-pm (concat "perl " exp-t))

     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm post-pl, after t again: %s" rs-from-pm))
     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm " mod-style )) ;; *NOT* ok 9

     )
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
