#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 10-perlnow-guess-run-string.t

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

;; o  perlnow-module-starter
;;    pm-file  (from current-buffer)
;;    perlnow-back-to-code
;;    t-file
;;    set-buffer pm
;;    perlnow-script
;;    guess run-string


;; TODO
;; back-up and exercise components of "guess" individually:
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

(defun et-greasy-monkees-warp-the-fundament ()
  "Yow."
  (interactive)

  (setq perlnow-force t) ;; ask me no questions
  (perlnow-tron)
  (let* (
       (funcname "perlnow-guess-run-string")
       (test-name (concat "Testing " funcname ))
       (test-context   "") ;; noncpan, modstar, h2xs...
       (test-ext-from  "") ;; pl, pm, or t
       (test-ext-new   "") ;; pl, pm, or t

       (test-mess      "") ;; for "new", from "from", in "context"
       (perl "/usr/bin/perl")
       )

  (let* (
       ;; module-starter (w/ Module::Build)
       (package-name-modstar "Planet::Ten")

       ;; pm: /home/doom/tmp/perlnow_test/dev/Planet-Ten/lib/Planet/Ten.pm
       (staging-area-modstar
        (perlnow-staging-area perlnow-dev-location package-name-modstar))
       (exp-pm-base-modstar "Ten.pm")
       (exp-pm-file-modstar
        (concat staging-area-modstar "lib"
                perlnow-slash "Planet" perlnow-slash exp-pm-base-modstar))

       ;; pl: /home/doom/tmp/perlnow_test/dev/Planet-Ten/bin/overthrust.pl
       (script-name-modstar "overthrust.pl")
       (exp-script-modstar
        (concat staging-area-modstar "bin" perlnow-slash script-name-modstar))

       ;; t: /home/doom/tmp/perlnow_test/dev/Planet-Ten/t/01-Planet-Ten.t
       (t-name-modstar "01-Planet-Ten.t")
       (exp-t-modstar
        (concat staging-area-modstar "t" perlnow-slash t-name-modstar))

       ;; old-style, deprecated
       pm-file-modstar          pm-buffer-modstar
       t-file-modstar           t-buffer-modstar
       script-file-modstar      script-buffer-modstar

       rs-from-script-modstar   exp-rs-from-pm-modstar
       rs-from-t-modstar        exp-rs-from-t-modstar
       rs-from-pm-modstar       exp-rs-from-pm-modstar

       ;; TODO rollout these
       pm-file          pm-buffer
       t-file           t-buffer
       script-file      script-buffer

       rs-from-script   exp-rs-from-pm
       rs-from-t        exp-rs-from-t
       rs-from-pm       exp-rs-from-pm
       )

    ;; "MODSTAR  module-starter (with Module::Build)
    (setq test-context "modstar")
    (if perlnow-debug (message (upcase test-context)))

    ;; get file names and buffers, first module and test file
    (perlnow-module-starter perlnow-dev-location package-name-modstar)
    (setq pm-file-modstar   (buffer-file-name))
    (setq pm-buffer-modstar (current-buffer))
    (perlnow-back-to-code)
    (setq t-file-modstar   (buffer-file-name))
    (setq t-buffer-modstar (current-buffer))

    ;; now, a script generated from the module
    ;; MODSTAR: script from pm
    (setq test-ext-from  "pm") ;; pl, pm, or t
    (setq test-ext-new   "pl") ;; pl, pm, or t
    (setq test-mess (concat test-ext-new " from " test-ext-from " in " test-context))

    (if perlnow-debug (message "XXX1: %s" test-mess))

    ;; back to the pm
    (set-buffer pm-buffer-modstar)
    ;; get rid of pre-existing script (non-interactive perlnow-script can't deal)
    (test-init-move-file-out-of-way exp-script-modstar)
    (perlnow-script exp-script-modstar)
    (setq script-file-modstar   (buffer-file-name))
    (setq script-buffer-modstar (current-buffer))

    ;; Some basic checks, verifying files were created as expected
    (if perlnow-debug (message "ts me: %s %s" t-file-modstar exp-t-modstar))
    (assert-equal t-file-modstar exp-t-modstar
                  (concat "Testing module-starter run, test file path: \n"
                          "        " t-file-modstar ))

    (assert-equal script-file-modstar exp-script-modstar
                  (concat "Testing perlnow-script for modstar, script path: \n"
                          "        " script-file-modstar ))

    (set-buffer pm-buffer-modstar)
    (setq exp-rs-from-pm-modstar
          "perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/t/01-Planet-Ten.t")
    (setq rs-from-pm-modstar (perlnow-guess-run-string))
    (if perlnow-debug (message "rs-from-pm-modstar: %s" rs-from-pm-modstar))

    (assert-equal rs-from-pm-modstar exp-rs-from-pm-modstar
                  (concat "Testing " funcname " for pm modstar "))

    (set-buffer script-buffer-modstar)
    (setq rs-from-script-modstar (perlnow-guess-run-string))
    (if perlnow-debug
        (message "rs-from-script-modstar: %s" rs-from-script-modstar))

    (setq exp-rs-from-script-modstar
          "/usr/bin/perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/bin/overthrust.pl")

    (assert-equal rs-from-script-modstar exp-rs-from-script-modstar
                  (concat "Testing " funcname " for script in modstar" ))

    (set-buffer t-buffer-modstar)
    (setq rs-from-t-modstar (perlnow-guess-run-string))
    (if perlnow-debug
        (message "rs-from-t-modstar: %s" rs-from-t-modstar))

    (setq exp-rs-from-t-modstar
          "perl /home/doom/tmp/perlnow_test/lib/Planet-Ten/t/01-Planet-Ten.t")

    (assert-equal rs-from-t-modstar exp-rs-from-t-modstar
                  (concat "Testing " funcname " for t in modstar" ))
    )


  (if perlnow-debug (message "H2XS"))

  (let* (
       ;; h2xs, ExtUtils::MakeMaker
       (package-name-h2xs "Hack::Excess")
       (staging-area-h2xs
        (perlnow-staging-area perlnow-dev-location package-name-h2xs))

        (exp-pm-base-h2xs "Excess.pm")
        (exp-pm-file-h2xs
         (concat staging-area-h2xs "lib"
                 perlnow-slash "Hack" perlnow-slash exp-pm-base-h2xs))

       (script-name-h2xs "lithium_perfume.pl")
       (exp-script-h2xs
        (concat staging-area-h2xs "bin" perlnow-slash script-name-h2xs))

       (t-name-h2xs "01-Hack-Excess.t") ;;
       (exp-t-h2xs
        (concat staging-area-h2xs "t" perlnow-slash t-name-h2xs))

       pm-file-h2xs pm-buffer-h2xs  t-file-h2xs t-buffer-h2xs
       script-file-h2xs script-buffer-h2xs

       rs-from-script-h2xs rs-from-t-h2xs rs-from-pm-h2xs
       exp-rs-from-pm-h2xs exp-rs-from-t-h2xs exp-rs-from-pm-h2xs
       )

  ;; TODO isn't the following all pretty mangled?
  ;; is this supposed to be a h2xs case? then the comment is wrong,
  ;; and running perlnow-module-starter is a mistake

  ;; For a module-starter/Module::Build based project,
  ;; get file names and buffers, first module and test file
  (perlnow-module-starter perlnow-dev-location package-name-h2xs)
  (setq pm-file-h2xs   (buffer-file-name))
  (setq pm-buffer-h2xs (current-buffer))
  (perlnow-back-to-code)
  (setq t-file-h2xs   (buffer-file-name))
  (setq t-buffer-h2xs (current-buffer))

  ;; now, a script generated from the module
  ;; back to the pm
  (perlnow-back-to-code)
  ;; move a pre-existing script out of the way
  ;; (noninteractive call to perlnow-script doesn't deal with this well...)
  (test-init-move-file-out-of-way exp-script-h2xs)

  (perlnow-script exp-script-h2xs)
  (setq script-file-h2xs   (buffer-file-name))
  (setq script-buffer-h2xs (current-buffer))

  ;; Some basic redundant checks, verifying files were created as expected
  (if perlnow-debug
      (message "t-file-x2xs: %s \nexp-t-h2xs: %s" t-file-h2xs exp-t-h2xs))
  (assert-t
   (string= t-file-h2xs exp-t-h2xs)
   (concat "Testing h2xs run, test file path: \n        " t-file-h2xs ))

  (assert-t
   (string= script-file-h2xs exp-script-h2xs)
   (concat "Testing perlnow-script for h2xs, script path: \n        " script-file-h2xs ))

  (set-buffer pm-buffer-h2xs)
  (setq exp-rs-from-pm-h2xs
        "perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/t/01-Hack-Excess.t")
  (setq rs-from-pm-h2xs (perlnow-guess-run-string))
  (if perlnow-debug
      (message "rs-from-pm-h2xs: %s \nexp-rs-from-pm-h2xs: %s" rs-from-pm-h2xs  exp-rs-from-pm-h2xs))
  (assert-t
   (string= rs-from-pm-h2xs exp-rs-from-pm-h2xs)
   (concat "Testing " funcname " for pm h2xs" ))

  (set-buffer script-buffer-h2xs)
  (if perlnow-debug
      (message "from script: %s" (pp (perlnow-guess-run-string))))
  (setq rs-from-script-h2xs (perlnow-guess-run-string))

  (setq exp-rs-from-script-h2xs
        "/usr/bin/perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/bin/lithium_perfume.pl")
  (assert-t
   (string= rs-from-script-h2xs exp-rs-from-script-h2xs)
   (concat "Testing " funcname " for script in h2xs" ))

  (set-buffer t-buffer-h2xs)
  (if perlnow-debug
      (message "from t: %s" (pp (perlnow-guess-run-string))))
  (setq rs-from-t-h2xs (perlnow-guess-run-string))

  (setq exp-rs-from-t-h2xs
        "perl /home/doom/tmp/perlnow_test/lib/Hack-Excess/t/01-Hack-Excess.t")

  (assert-t
   (string= rs-from-t-h2xs exp-rs-from-t-h2xs)
   (concat "Testing " funcname " for t in h2xs" ))

  )

  (if perlnow-debug (message "NONCPAN"))

  (let* (
       ;; stand-alone module created with object template
       (package-name-modoop "Modoc::Aims")
       (exp-pm-base-modoop "Aims.pm")
       (exp-pm-file-modoop
        (concat perlnow-pm-location "Modoc"
                perlnow-slash exp-pm-base-modoop))

       (script-name-modoop "cube.pl")
       (exp-script-modoop
        (concat perlnow-script-location script-name-modoop))

       ;; TODO
;;        (t-name-modoop "01-Hack-Excess.t") ;;
;;        (exp-t-modoop
;;         (concat perlnow-t-location t-name-modoop))

       pm-file-modoop    pm-buffer-modoop  t-file-modoop t-buffer-modoop
       script-file-modoop   script-buffer-modoop

       rs-from-script-modoop rs-from-t-modoop rs-from-pm-modoop
       exp-rs-from-pm-modoop exp-rs-from-t-modoop exp-rs-from-pm-modoop

;;        (package-name-modexp "Torr::Rivet")
;;        (exp-pm-base-modexp "Rivet.pm")
;;        (exp-pm-file-modexp
;;         (concat perlnow-pm-location "Torr"
;;            perlnow-slash exp-pm-base-modexp))

       ;; where are testfiles *supposed* to end up for non cpan?
       ;; effectively in a similar local: a t next to the incspot.

       )

;;; TODO

  ) ;; end let*
));; end defun
(et-greasy-monkees-warp-the-fundament)


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
