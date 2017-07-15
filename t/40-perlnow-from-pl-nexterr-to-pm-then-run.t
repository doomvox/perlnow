#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script

;; 40-perlnow-from-pl-nexterr-to-pm-then-run.t

;; Test story:

;; Initialize the t40 tree with script and module in dat/code
;; Open the script.
;;   Check "status" vars (?)
;; Do a perlnow-run: but still have to tell it what to do explicitly.
;;   Check *compilation* for the uninitialized warning (?)
;; Do a next-error (Q: you can do this *from the script* right?)
;; Check that you opened the *.pm file.
;;   Check "status" vars (?)
;;   ... Now it gets awkward.
;;       the bug is in perlnow-run's guess.
;;       but we can't fake an interactive call easily.
;;   Maybe: run the "guess" code standalone.
;;   Maybe: do this with the other perlnow-run, to review the technique.
;;          See older tests that check guess code directly.
;;   Maybe: bite bullet and re-write perlnow-run for testability.

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
   (perlnow-tron)
   (let* (
          (test-loc (test-init)) ;; TODO need to use a lib and bin in there
          (test-name "Testing next-error to pm from pl")
     ;;    case-name

          (package-name "Trash::Mountain")
          (script-name "timber.pl")
          (script-base "timber")
          (pm-base "Mountain")

          (data-loc (concat default-directory "dat" test-init-slash))
          (source-loc (concat data-loc "code" test-init-slash "s40" test-init-slash))
           ;; /home/doom/End/Cave/Perlnow/lib/perlnow/elisp/t/dat/code
          (source-pl  (concat source-loc "bin" test-init-slash "timber.pl"))
          (source-pm  (concat source-loc "lib" test-init-slash "Trash" test-init-slash "Mountain.pm"))

          (pm-loc  (concat perlnow-pm-location "Trash" perlnow-slash))
          (pm-file (concat pm-loc  pm-base ".pm"))
          (script-loc perlnow-script-location)
          (script-file (concat script-loc script-name))

          (expected-t-location
           (test-init-fixdir (concat perlnow-pm-location "../t")))

          (expected-t-file
           (concat expected-t-location "01-" script-base "-script.t"))

          ;; (perl "/usr/bin/perl") ;; same has hash-bang in script template
          (perl "perl") ;; but much better to let the PATH sort it out

          pm-buffer script-buffer t-buffer argument-text opened-file comp-buffer
          )
     ;; (setq case-name "Testing perlnow-run of script")

     ;; Initialize the t40 tree with script and module in dat/code
     (copy-file source-pl script-loc t)
     (test-init-mkpath pm-loc)
     (copy-file source-pm pm-loc t)

     ;; open the script.
     (find-file script-file)
     (setq script-buffer (current-buffer))

     (message "script 2: %s" (perlnow-vars-report-string))

     (assert-nil
      perlnow-associated-code
      "Testing newly opened script has no association")

     (assert-nil
      perlnow-run-string
      "Testing newly opened script has no run-string")

     ;; Do a perlnow-run: but still have to tell it what to do explicitly.
     ;;    (funcall-interactively 'perlnow-run) ;; wrong number of arguments
     (setq perlnow-run-string
           (concat perl " " script-file))
     (if perlnow-debug
         (message "perlnow-run-string: %s\n" perlnow-run-string))
         ;; /usr/bin/perl /home/doom/tmp/perlnow_test/t38/bin/timber.pl
     (perlnow-run perlnow-run-string)

     (sleep-for 2) ;; wait for compile (only thing in emacs that's async)

     ;; The compilation window should show warning:
     ;;  Use of uninitialized value $arg in print at ... Mountain.pm line 62.
     (switch-to-buffer "*compilation*")
     (setq comp-buffer (current-buffer))
     (let* ((compilation-results (buffer-string) )
            (warning-prefix  "Use of uninitialized value")
            (trash-out-pat (concat "^" warning-prefix ))
             ;; Could use these too, but it's testing the wrong things
             ;; ( warning-middle "arg in print at")
             ;; ( warning-suffix "Mountain.pm line 62")
            )
       (assert-t (string-match trash-out-pat compilation-results 1)
                 (concat "Testing perlnow-run of script worked, and generated warning"))
       )

     ;; We'll do a next-error to jump to the pm file generating that warning
     (switch-to-buffer script-buffer)

     ;; pass value to next-error-hook
     (setq perlnow-last-buffer-file-name (buffer-file-name))
     (next-error 1 t) ;; the 'reset' of t means take it from top
     ;;     (next-error)
     ;;     (funcall-interactively 'next-error)

     ;; Presuming no other buffers for similar files, guess the name
     (let* ((new-pm-buffer-name (concat pm-base ".pm") ))
       ;; (Why we *need* to do this switch buffer is a mystery of next-error.
       ;; Must. Stop. Thinking. About. This.)
       (switch-to-buffer  new-pm-buffer-name))

     ;; Check that you now have the *.pm file (newly opened, right?)
     (setq opened-file (buffer-file-name))
     (assert-t opened-file
               "Testing that next-error opened a buffer that has a file name")
     (assert-equal pm-file opened-file
                   "Testing next-error opened pm file from script")
     (setq pm-buffer (current-buffer))

     ;; Check some "status" vars
     (message "script 1: %s" (perlnow-vars-report-string))

     (assert-nil
      perlnow-run-string
      "Testing newly opened module has no run-string")

     ;; check the "guess" for this *.pm (lifted from 12-*.t)
     (switch-to-buffer pm-buffer)

     ;; "perl /home/doom/tmp/perlnow_test/t48/bin/timber.pl"
     (setq exp-rs-from-pm (concat "perl " perlnow-script-location "timber.pl"))
     (setq rs-from-pm (perlnow-guess-run-string))
     (if perlnow-debug (message "rs-from-pm: %s" rs-from-pm))

     (assert-equal exp-rs-from-pm rs-from-pm
           (concat "Testing perlnow-guess-run-string for pm opened from script by next-error"))

      (switch-to-buffer pm-buffer)
      (assert-equal
        script-file perlnow-associated-code
        "Testing module after perlnow-next-error associated with script.")

      (switch-to-buffer script-buffer)
      (assert-equal
        pm-file perlnow-associated-code
        "Testing script after perlnow-next-error associated with module.")

     ;; TODO maybe do a perlnow-run (or another guess) and check that again.

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

;;; 40-perlnow-from-pl-nexterr-to-pm-then-run.t  ends here
