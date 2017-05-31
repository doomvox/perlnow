#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 38-perlnow-from-pm-to-script-to-t.t

;; A test story for you:

;; Beginning with same as 07-perlnow-from-pm-to-script-and-run.t:

;; Create a module.
;;   Add a tiny sub the code, have it echo the given
;; Create a script using the module.
;;   Add a line, takes first argument, passes it to the echo sub
;; Use
;;   perlnow-set-run-string
;;   to add an argument value to the run string.

;; Now we deviate from 07:

;; Create a test for the script (using "edit"):
;;   Add the expected return value
;; Check the name of the test script.
;; Run the test script.
;;   Check for output of ok and not "not ok" in *compilation*.

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
          (test-loc (test-init))
          (test-name "")
          (package-name "Trash::Mountain")
          (script-name "timber.pl")
          (script-base "timber")
          (expected-pm-base "Mountain")

          (expected-pm-file
            (concat perlnow-pm-location "Trash" perlnow-slash expected-pm-base ".pm"))
          (expected-script
           (concat perlnow-script-location script-name))

;;           (sub-code-str
;;            "sub yodel {my $arg=shift; print $arg ,\"\n\";}")
          (sub-code-core-str "  print $arg ,\"\\n\";") ;; insert after perlnow-insert-sub
          (calling-code-str "yodel($ARGV[0]);")

          (expected-t-location
           (test-init-fixdir (concat perlnow-pm-location "../t")))

          (expected-t-file
           (concat expected-t-location "01-" script-base "-script.t"))

          (perl "/usr/bin/perl") ;; same as hash-bang in script template

          pm-buffer script-buffer t-buffer argument-text
          )
     (setq test-name "Testing perlnow-module")
     (perlnow-module perlnow-pm-location package-name)
     (setq pm-buffer (current-buffer))

     (perlnow-insert-sub "yodel")
     (insert sub-code-core-str)
     (save-buffer)

     ;; The pm file should exist on disk now.
     (assert-t
      (file-exists-p expected-pm-file)
      (concat test-name ": created file:\n       " expected-pm-file ))

     (assert-t
      (perlnow-module-file-p (buffer-file-name))
      "Testing perlnow-module-file-p to confirm module looks like a module.")

     (assert-nil
      (perlnow-script-file-p (buffer-file-name))
      "Testing perlnow-script-file-p to confirm module is not like script.")

     ;; move a pre-existing script out of the way
     ;; (noninteractive call to perlnow-script doesn't deal with this well...)
     (test-init-move-file-out-of-way expected-script)
     (setq test-name "Testing perlnow-script")
     (perlnow-script expected-script)
     (setq script-buffer (current-buffer))

     (insert calling-code-str)
     (save-buffer)

     ;; now the script file should exist on disk
     (assert-t
      (file-exists-p expected-script)
      (concat test-name ": generated expected script: " script-name))

     (assert-t
      (perlnow-script-file-p (buffer-file-name))
      "Testing perlnow-script-file-p to confirm script looks like script.")

     (assert-nil
      (perlnow-module-file-p (buffer-file-name))
      "Testing perlnow-module-file-p to confirm script is not like module.")

     (setq test-name "Testing perlnow-run")

     ;; TODO These need work to get closer to how you'd use them interactively
     (setq argument-text "Sunnyvale, ho!!")
     (setq perlnow-run-string
           (concat perl " " expected-script "  '" argument-text "'"))
     (if perlnow-debug
         (message "perlnow-run-string: %s\n" perlnow-run-string))
         ;; /usr/bin/perl /home/doom/tmp/perlnow_test/t38/bin/timber.pl  'Sunnyvale, ho!!'
     (perlnow-run perlnow-run-string)
     (sleep-for 2) ;; wait for compile (only thing in emacs that's async)

     ;; The compilation window should show the string "Sunnyvale, ho!!" (in first column):
     ;;
     ;;     -*- mode: compilation; default-directory: "~/bin/" -*-
     ;;     Compilation started at Mon Jan 23 17:10:04
     ;;
     ;;     /usr/bin/perl /home/doom/bin/begoff Sunnyvale, ho!!
     ;;     Sunnyvale, ho!!
     ;;
     ;;     Compilation finished at Mon Jan 23 17:10:04

     (set-buffer "*compilation*")
     (let* ( ( compilation-results (buffer-string) )
             ( trash-out-pat (concat "^" argument-text))  ;; "^Sunnyvale, ho!!")
             check-ok-p
             )
       (if perlnow-debug (message "07"))
       (setq check-ok-p
             (assert-t
              (string-match trash-out-pat compilation-results)
              (concat "Testing perlnow-run of script worked, and generated output")))
       )

     ;; Create a test for the script, using "edit-test":
     (set-buffer script-buffer)   ;; TODO right around here, bombs when stepping through with edebug
                                  ;;      Q: would switch-buffer be better?
     ;; Faking a simple "edit-test" call, without arguments
              (perlnow-open-test-file
               (perlnow-get-test-file-name))

     (setq t-buffer (current-buffer))
     (setq t-file-name (buffer-file-name))
     (if perlnow-debug (message "08"))
     (assert-equal expected-t-file t-file-name
                   (concat test-name ": expected test name created by edit-test"))

     ;; Fill in the same string in EXPECTED and as script argument in the new test file:
     (setq argument-text "Smell *that*, Mountain View!")

        ;; # TODO Enter the expected output from the script
        ;; my $expected=<<"EXPECTED";
        ;; (>>>POINT<<<)
        ;; EXPECTED

        ;;   # TODO any arguments to add after the script name?
        ;;   my $result = qx{ $script_name };

     ;; Presuming we're in the EXPECTED block:
     (insert argument-text)

     ;; Navigate to just inside the qx{} construct, and insert the same string
     (let* ((quoted-arg-text (concat " '" (shell-quote-argument argument-text) "'"))
            )
       ;; Navigate to just inside the qx{} construct, and insert the same string
       (search-forward "result")
       (search-forward "qx{")
       (forward-char -1)
       (forward-sexp 1)
       (forward-char -2)
       (insert "  ")
       (insert quoted-arg-text)
       (save-buffer)
       )

     ;; Run the test...
     (setq perlnow-run-string
           (concat perl " " expected-t-file ))

     (if perlnow-debug
         (message "perlnow-run-string: %s\n" perlnow-run-string))

     (perlnow-run perlnow-run-string)
     (sleep-for 2) ;; wait for compile (only thing in emacs that's async)

     ;; Scrape *compilation* window for this:
         ;; ok 2 - Testing script timber.pl

     (set-buffer "*compilation*")
     (let* ( ( compilation-results (buffer-string) )
             ( expected-output-string "ok 2 - Testing script timber.pl")
             ( trash-out-pat (concat "^" expected-output-string))
             check-ok-p
             )
       (if perlnow-debug
           (message "compilation-results:\n %s" compilation-results))
       (if perlnow-debug (message "09"))
       (setq check-ok-p
             (assert-t
              (string-match trash-out-pat compilation-results)
              (concat "Testing that the perlnow-run of a perl test worked")))
       )
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
