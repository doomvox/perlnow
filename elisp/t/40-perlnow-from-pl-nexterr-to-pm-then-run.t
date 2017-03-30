#!/usr/local/bin/emacs --script
;; /usr/bin/emacs

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
          (test-name "")

          (package-name "Trash::Mountain")
          (script-name "timber.pl")
          (script-base "timber")
          (pm-base "Mountain")

          ;; TODO test-init-slash
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

          (perl "/usr/bin/perl") ;; same as hash-bang in script template

          pm-buffer script-buffer t-buffer argument-text opened-file comp-buffer

          )
     (message "Zzzzziiiinnnnggg...")
     (setq test-name "Testing perlnow-run of script") ;; TODO better handling of test-name, eh?

     ;; Initialize the t40 tree with script and module in dat/code
;;     (copy-file source-pl script-file t)
     (copy-file source-pl script-loc t)
;;     (copy-file source-pm pm-file t)
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
     (setq perlnow-run-string
           (concat perl " " script-file))
     (if perlnow-debug
         (message "perlnow-run-string: %s\n" perlnow-run-string))
         ;; /usr/bin/perl /home/doom/tmp/perlnow_test/t38/bin/timber.pl
     (perlnow-run perlnow-run-string)
     (sleep-for 2) ;; wait for compile (only thing in emacs that's async)

     ;; The compilation window should show warning
     ;;  Use of uninitialized value $arg in print at ... Mountain.pm line 62.
     (switch-to-buffer "*compilation*")
     (setq comp-buffer (current-buffer))
     (let* ((compilation-results (buffer-string) )
            (warning-prefix  "Use of uninitialized value")
            (trash-out-pat (concat "^" warning-prefix ))
             ;; ( warning-middle "arg in print at")
             ;; ( warning-suffix "Mountain.pm line 62")
            )
;;        (assert-matches trash-out-pat compilation-results
;;                        (concat "Testing perlnow-run of script worked, and generated warning"))

       (assert-t (string-match trash-out-pat compilation-results 1)
                 (concat "Testing perlnow-run of script worked, and generated warning"))
       )
     ;; Do a next-error (Q: you can do this *from the script* right?)

;;     (switch-to-buffer comp-buffer)
     ;; (goto-char (point-min))
     (switch-to-buffer script-buffer)
     (next-error 1 t) ;; the 'reset' of t means take it from top

     ;; Presuming there are no other buffers for similar files,
     ;; we can guess the name of the newly opened pm
     (switch-to-buffer (concat pm-base ".pm"))

     ;; Check that you now have the *.pm file (newly opened, right?)
     (setq opened-file (buffer-file-name))
     (assert-t opened-file
               "Testing that next-error opened a buffer with file name")
     (assert-equal pm-file opened-file
                   "Testing next-error opened pm file from script")
     (setq pm-buffer (current-buffer))

     ;; Check some "status" vars
     (message "script 1: %s" (perlnow-vars-report-string))

     ;; TODO would rather have this pm associated with script now.  can fix?
     (assert-nil
      perlnow-associated-code
      "Testing newly opened module has no association")

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



;;      ;; TODO is the pm associated with script yet? (For some reason, it has a *.t association. OK?)
;;      ;; perlnow-associated-code:
;;      ;;   /home/doom/tmp/perlnow_test/t40/t/01-Trash-Mountain-yodel.t
;;      (switch-to-buffer pm-buffer)
;;      (assert-equal
;;        script-file perlnow-associated-code
;;        "Testing module after guess associated with script.")


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
