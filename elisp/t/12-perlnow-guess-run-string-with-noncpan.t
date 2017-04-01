#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 12-perlnow-guess-run-string-with-noncpan.t

;; Test story:

;; The overall idea is to create a bunch of different kinds of
;; perl files in different structures, then check the result of
;; perlnow-guess-run-string in all of them.

;; This context:
;;   non-cpan-style, perlnow-object-module
;;
;; Complication: *history* matters.
;; The run-string of a pm reflects what was just created.

;; The story in more detail:
;;  open a module, then edit a test file. and back in the module buffer:
;;    perlnow-guess-run-string
;;  verify it runs the new *.t file
;;  from the module buffer, create a script
;;  (verify it created the expected script file)
;;  from the module buffer, perlnow-guess-run-string
;;  verify it runs the newly created script
;;  from the script buffer, run perlnow-guess-run-string
;;  verfiy this also runs the script.
;;  from the test buffer, run  perlnow-guess-run-string
;;  verify it runs the test file
;;  back to the pm file:
;;    did guess in *.t file change the runstring here?  Should be no (I think).
;;  back to the pm file:  do an edit-test
;;    verify that changes run-string to the *.t

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
          (mod-style   "object-style module") ;; modstar, h2xs, object, exporter
          (test-ext-from  "") ;; pl, pm, or t
          (test-ext-new   "") ;; pl, pm, or t
          (test-mess      "") ;; for "new", from "from", in "context"
          (perl "/usr/bin/perl")  ;; should match script template hashbang

          ;; stand-alone module created with object template
          (package-name "Modoc::Aims")

          (exp-pm-base "Aims.pm")
          (exp-pm-file
           (concat perlnow-pm-location "Modoc"
                   perlnow-slash exp-pm-base))

          (script-name "cube.pl")
          (exp-script
           (concat perlnow-script-location script-name))

          (t-name "01-Modoc-Aims.t")
          ;; presume policy of "../t"
          ;; (( but this isn't quite it ))
          ;; (t-loc  (concat (perlnow-fixdir (concat perlnow-pm-location "..")) "t/"))
          (exp-t  (concat test-loc "t" perlnow-slash t-name))

          pm-file          pm-buffer
          t-file           t-buffer
          script-file      script-buffer

          rs-from-script   exp-rs-from-pm
          rs-from-t        exp-rs-from-t
          rs-from-pm       exp-rs-from-pm
          )
     (message "test-loc: %s" test-loc)
     (message "exp-t:    %s" exp-t)

     ;; "OBJECT
     (if perlnow-debug (message (upcase mod-style)))

     (perlnow-object-module perlnow-pm-location package-name)
     (setq pm-file   (buffer-file-name))
     (setq pm-buffer (current-buffer))
     (assert-t
      (perlnow-module-code-p)
      "Testing that perlnow-object-module left new pm buffer active.") ;; ok 1

     ;; Faking a simple "edit-test" call, without arguments
              (perlnow-open-test-file
               (perlnow-get-test-file-name))
     (setq t-file   (buffer-file-name))
     (setq t-buffer (current-buffer))

     (assert-equal exp-t t-file
                   (concat "Testing perlnow-edit-test created test file: \n"
                           "        " t-file )) ;; ok 2

     (setq test-ext-from  "pm") ;; pl, pm, or t
     (setq test-ext-new   "pl") ;; pl, pm, or t
     (setq test-mess (concat test-ext-new " from " test-ext-from " from pm style: " mod-style))

     ;; back to the pm
     (set-buffer pm-buffer)

     ;; guessing a run-string, starting from a raw module
     (setq exp-rs-from-pm (concat "perl " exp-t ))
     (setq rs-from-pm (perlnow-guess-run-string))
     ;; (message "ZING rs-from-pm: %s" rs-from-pm)

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
                   (format "Testing perlnow-script from %s: \n %s" mod-style script-file)) ;; ok 4


     ;; back to the module again, and guess
     (set-buffer pm-buffer)

     ;; "perl /home/doom/tmp/perlnow_test/bonk12/bin/cube.pl"
     (setq exp-rs-from-pm (concat "perl " perlnow-script-location "cube.pl"))
     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm post-pl: %s" rs-from-pm))

     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm, (" mod-style ") after script ")) ;; ok 5

     (set-buffer script-buffer)
     (setq rs-from-script (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-script: %s" rs-from-script))

     ;; "/usr/bin/perl /home/doom/tmp/perlnow_test/dev/Planet-Ten/bin/overthrust.pl"
     (setq exp-rs-from-script (concat "/usr/bin/perl " perlnow-script-location "cube.pl"))
     (assert-equal exp-rs-from-script rs-from-script
                   (concat "Testing " funcname " for script from " mod-style )) ;; ok 6


     ;; switch to test file, and guess
     (set-buffer t-buffer)
     (setq rs-from-t (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-t: %s" rs-from-t))

     ;; "perl /home/doom/tmp/perlnow_test/bonk12/t/01-Modoc-Aims.t"
     (setq exp-rs-from-t (concat "perl " exp-t))
     (assert-equal exp-rs-from-t rs-from-t
                   (concat "Testing " funcname " for t for an " mod-style ))  ;; ok 7

     ;; back to pm, did guess in t change rs here?
     (set-buffer pm-buffer)
     ;; after we've done a guess in a *.t, it *doesn't* change the rs for the pm:
     (setq exp-rs-from-pm
           (concat "perl " perlnow-script-location "cube.pl"))

     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm post-pl, after t again: %s" rs-from-pm))
     (assert-equal exp-rs-from-pm rs-from-pm
                   (concat "Testing " funcname " for pm (" mod-style ")" )) ;; ok 8

     ;; just to make sure
     (set-buffer pm-buffer)
     ;; (perlnow-edit-test-file exp-t)
     ;; (perlnow-edit-test-file 1 exp-t)
     (perlnow-open-test-file exp-t) ;; faking an "edit-test" call
     ;; and back to the generating pm buffer
     (set-buffer pm-buffer)

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
