#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 10-perlnow-guess-run-string.t

;; Test story:

;; o  create a bunch of different kinds of perl files in different
;;    structures.  Save file paths and/or buffers.

;; o  check the result of perlnow-guess-run-string in all of them.

;; o  exercise components of "guess" individually (maybe do this first?)
;;      perlnow-find-cpan-style-staging-area
;;      perlnow-cpan-style-test-run-string
;;      perlnow-test-run-string-harder
;;      perlnow-generate-run-string
;;      perlnow-generate-run-string-and-associate
;;      perlnow-latest-test-file
;;      perlnow-list-test-files

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(if (file-exists-p "test-init-elisp.el")
    (load-file "test-init-elisp.el"))

(setenv "USE_TAP" "t")

;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
;; TODO
;;  install the latest, maybe via emacs package management: should have my fix.
(load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")

(test-simple-start) ;; Zero counters and start the stop watch.

(setq perlnow-force t) ;; ask me no questions

(let* (
       (funcname "perlnow-guess-run-string")
       (test-name (concat "Testing " funcname ))

       ;; module-stater, Module::Build
       (package-name-msmb "Planet::Ten")
       (staging-area-msmb
        (perlnow-staging-area perlnow-pm-location package-name-msmb))
       (expected-pm-base-msmb "Ten.pm")
       (expected-pm-file-msmb
        (concat staging-area-msmb "lib"
                perlnow-slash "Planet" perlnow-slash expected-pm-base-msmb))

       (script-name-msmb "overthrust.pl")
       (expected-script-msmb
        (concat staging-area-msmb "bin" perlnow-slash script-name-msmb))

       (t-name-msmb "01-Planet-Ten.t")
       (expected-t-msmb
        (concat staging-area-msmb "t" perlnow-slash t-name-msmb))

       pm-file-msmb pm-buffer-msmb  t-file-msmb t-buffer-msmb
       script-file-msmb script-buffer-msmb

       runstring-from-script-msmb
       runstring-from-t-msmb
       runstring-from-pm-msmb

       expected-rs-from-pm-msmb
       expected-rs-from-t-msmb
       expected-rs-from-pm-msmb


       ;; h2xs, ExtUtils::MakeMaker
       (package-name-h2xs "Hack::Excess")
       (staging-area-h2xs
        (perlnow-staging-area perlnow-pm-location package-name-h2xs))

        (expected-pm-base-h2xs "Excess.pm")
        (expected-pm-file-h2xs
         (concat staging-area-h2xs "lib"
                 perlnow-slash "Hack" perlnow-slash expected-pm-base-h2xs))

       (script-name-h2xs "lithium_perfume.pl")
       (expected-script-h2xs
        (concat staging-area-h2xs "bin" perlnow-slash script-name-h2xs))

       (t-name-h2xs "01-Hack-Excess.t") ;;
       (expected-t-h2xs
        (concat staging-area-h2xs "t" perlnow-slash t-name-h2xs))

       pm-file-h2xs pm-buffer-h2xs  t-file-h2xs t-buffer-h2xs
       script-file-h2xs script-buffer-h2xs

       runstring-from-script-h2xs
       runstring-from-t-h2xs
       runstring-from-pm-h2xs

       expected-rs-from-pm-h2xs
       expected-rs-from-t-h2xs
       expected-rs-from-pm-h2xs

       ;; stand-alone module created with object template
       (package-name-modoop "Modoc::Aims")
       (expected-pm-base-modoop "Aims.pm")
       (expected-pm-file-modoop
        (concat perlnow-pm-location "Modoc"
                perlnow-slash expected-pm-base-modoop))

       (script-name-modoop "cube.pl")
       (expected-script-modoop
        (concat perlnow-script-location script-name-modoop))


       ;; TODO
;;        (t-name-modoop "01-Hack-Excess.t") ;;
;;        (expected-t-modoop
;;         (concat perlnow-t-location t-name-modoop))

       pm-file-modoop pm-buffer-modoop  t-file-modoop t-buffer-modoop
       script-file-modoop script-buffer-modoop

       runstring-from-script-modoop
       runstring-from-t-modoop
       runstring-from-pm-modoop

       expected-rs-from-pm-modoop
       expected-rs-from-t-modoop
       expected-rs-from-pm-modoop

;;        (package-name-modexp "Torr::Rivet")
;;        (expected-pm-base-modexp "Rivet.pm")
;;        (expected-pm-file-modexp
;;         (concat perlnow-pm-location "Torr"
;;            perlnow-slash expected-pm-base-modexp))

       ;; where are testfiles *supposed* to end up for non cpan?
       ;; effectively in a similar local: a t next to the incspot.


       (perl "/usr/bin/perl")
       )

  ;; For a module-starter/Module::Build based project,
  ;; get file names and buffers, first module and test file
  (perlnow-module-starter perlnow-pm-location package-name-msmb)
  (setq pm-file-msmb   (buffer-file-name))
  (setq pm-buffer-msmb (current-buffer))
  (next-buffer)
  (setq t-file-msmb   (buffer-file-name))
  (setq t-buffer-msmb (current-buffer))

  ;; now, a script generated from the module
  (next-buffer) ;; back to the pm
  ;; move a pre-existing script out of the way
  ;; (noninteractive call to perlnow-script doesn't deal with this well...)
  (let* ((filename expected-script-msmb)
         (backup-name (concat filename ".OLD")) )
   (cond ((file-exists-p filename)
          (message "10*.t: renaming existing file: %s" filename)
          (rename-file filename backup-name t) ;; overwrites
          )))

  (perlnow-script expected-script-msmb)
  (setq script-file-msmb   (buffer-file-name))
  (setq script-buffer-msmb (current-buffer))

  ;; Some basic redundant checks, verifying files were created as expected
  ;; (message "ts me: %s %s" t-file-msmb expected-t-msmb);; DEBUG
  (assert-t
   (string= t-file-msmb expected-t-msmb)
   (concat "Testing module-starter run, test file path: \n"
           "        " t-file-msmb ))

  (assert-t
   (string= script-file-msmb expected-script-msmb)
   (concat "Testing perlnow-script for msmb, script path: \n"
           "        " script-file-msmb ))

  (set-buffer pm-buffer-msmb)
  (message "from pm: %s" (pp (perlnow-guess-run-string)))
  (setq expected-rs-from-pm-msmb
        "perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/t/01-Planet-Ten.t")
  (setq runstring-from-pm-msmb (perlnow-guess-run-string))
  (message "runstring-from-pm-msmb: %s" runstring-from-pm-msmb);; DEBUG

  (assert-t
   (string= runstring-from-pm-msmb expected-rs-from-pm-msmb)
   (concat "Testing " funcname " for pm msmb "
;;            "\n"
;;            "runstring-from-pm-msmb: "   runstring-from-pm-msmb "\n"
;;            "expected-rs-from-pm-msmb: " expected-rs-from-pm-msmb "\n"
           ))

  (set-buffer script-buffer-msmb)
  (message "from script: %s" (pp (perlnow-guess-run-string)))
  (setq runstring-from-script-msmb (perlnow-guess-run-string))
  (message "runstring-from-script-msmb: %s" runstring-from-script-msmb);; DEBUG

  (setq expected-rs-from-script-msmb
        "/usr/bin/perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/bin/overthrust.pl")
  (assert-t
   (string= runstring-from-script-msmb expected-rs-from-script-msmb)
   (concat "Testing " funcname " for script in msmb" ))

  (set-buffer t-buffer-msmb)
  (message "from t: %s" (pp (perlnow-guess-run-string)))
  (setq runstring-from-t-msmb (perlnow-guess-run-string))
  (message "runstring-from-t-msmb: %s" runstring-from-t-msmb);; DEBUG

  (setq expected-rs-from-t-msmb
        "perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/t/01-Planet-Ten.t")

  (assert-t
   (string= runstring-from-t-msmb expected-rs-from-t-msmb)
   (concat "Testing " funcname " for t in msmb" ))

  ;; For a module-starter/Module::Build based project,
  ;; get file names and buffers, first module and test file
  (perlnow-module-starter perlnow-pm-location package-name-h2xs)
  (setq pm-file-h2xs   (buffer-file-name))
  (setq pm-buffer-h2xs (current-buffer))
  (next-buffer)
  (setq t-file-h2xs   (buffer-file-name))
  (setq t-buffer-h2xs (current-buffer))

  ;; now, a script generated from the module
  (next-buffer) ;; back to the pm
  ;; move a pre-existing script out of the way
  ;; (noninteractive call to perlnow-script doesn't deal with this well...)
  (let* ((filename expected-script-h2xs)
         (backup-name (concat filename ".OLD")) )
   (cond ((file-exists-p filename)
          (message "10*.t: renaming existing file: %s" filename)
          (rename-file filename backup-name t) ;; overwrites
          )))

  (perlnow-script expected-script-h2xs)
  (setq script-file-h2xs   (buffer-file-name))
  (setq script-buffer-h2xs (current-buffer))

  ;; Some basic redundant checks, verifying files were created as expected
  ;; (message "ts me: %s %s" t-file-h2xs expected-t-h2xs);; DEBUG
  (assert-t
   (string= t-file-h2xs expected-t-h2xs)
   (concat "Testing h2xs run, test file path: \n        " t-file-h2xs ))

  (assert-t
   (string= script-file-h2xs expected-script-h2xs)
   (concat "Testing perlnow-script for h2xs, script path: \n        " script-file-h2xs ))

  (set-buffer pm-buffer-h2xs)
  (message "from pm: %s" (pp (perlnow-guess-run-string)))
  (setq expected-rs-from-pm-h2xs
        "perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/t/01-Hack-Excess.t")
  (setq runstring-from-pm-h2xs (perlnow-guess-run-string))

  (assert-t
   (string= runstring-from-pm-h2xs expected-rs-from-pm-h2xs)
   (concat "Testing " funcname " for pm h2xs" ))

  (set-buffer script-buffer-h2xs)
  (message "from script: %s" (pp (perlnow-guess-run-string)))
  (setq runstring-from-script-h2xs (perlnow-guess-run-string))

  (setq expected-rs-from-script-h2xs
        "/usr/bin/perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/bin/lithium_perfume.pl")
  (assert-t
   (string= runstring-from-script-h2xs expected-rs-from-script-h2xs)
   (concat "Testing " funcname " for script in h2xs" ))

  (set-buffer t-buffer-h2xs)
  (message "from t: %s" (pp (perlnow-guess-run-string)))
  (setq runstring-from-t-h2xs (perlnow-guess-run-string))

  (setq expected-rs-from-t-h2xs
        "perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/t/01-Hack-Excess.t")

  (assert-t
   (string= runstring-from-t-h2xs expected-rs-from-t-h2xs)
   (concat "Testing " funcname " for t in h2xs" ))


;;; TODO


  ) ;; end let*

(end-tests)

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
