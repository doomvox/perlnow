#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 07-perlnow-from-pm-to-script-and-run.t

;; A test story for you:

;; Create a module.
;;   Add a tiny sub the code, have it echo the given
;; Create a script using the module.
;;   Add a line, takes first argument, passes it to the echo sub
;; Use
;;   perlnow-set-run-string
;;   to add an argument value to the run string.
;; run the script file.
;; See if the value gets echoed to the *compilation* buffer.

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
          (package-name "Garbagio::Fantastico")
          (script-name "fill_can.pl")
          (expected-pm-base "Fantastico.pm")
          (expected-pm-file
           (concat perlnow-pm-location "Garbagio" perlnow-slash expected-pm-base))
          (expected-script
           (concat perlnow-script-location script-name))
          (sub-code-core-str "  print $arg, \"\\n\";") ;; Use with perlnow-insert-sub
          (calling-code-str
           "  echo( $ARGV[ 0 ] );")
          (perl "/usr/bin/perl") ;; same has hash-bang in script template
          pm-buffer
          )
     (setq test-name "Testing perlnow-module")
     (if perlnow-debug (message "RIDEON: %s" test-name))
     (perlnow-module perlnow-pm-location package-name)

     (perlnow-insert-sub "echo")
     (insert sub-code-core-str)
     (save-buffer)

     (setq pm-buffer (current-buffer))

     ;; The pm file should exist on disk now.
     (assert-t
      (file-exists-p expected-pm-file)
      (concat test-name ": created file:\n       " expected-pm-file )) ;; ok 1

     (assert-t
      (perlnow-module-file-p (buffer-file-name))
      "Testing perlnow-module-file-p to confirm module looks like a module.") ;; ok 2

     (assert-nil
      (perlnow-script-file-p (buffer-file-name))
      "Testing perlnow-script-file-p to confirm module is not like script.")

     ;; move a pre-existing script out of the way
     ;; (noninteractive call to perlnow-script doesn't deal with this well...)
     (test-init-move-file-out-of-way expected-script)

     (setq test-name "Testing perlnow-script")
     (if perlnow-debug (message "STUMBLE on BUM: %s" test-name))

     (set-buffer pm-buffer)
     (perlnow-script expected-script)
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
     (if perlnow-debug (message "SMELL_O_DOOM: %s" test-name))

     ;; TODO These need work to get closer to how you'd use them interactively
     (setq perlnow-run-string
           (concat perl " " expected-script " TRASH!"))
     (if perlnow-debug (message "pnr: %s" perlnow-run-string))

     (perlnow-run perlnow-run-string)

     ;; The compilation window should show the string "TRASH!" (in first column):
     ;;
     ;;     -*- mode: compilation; default-directory: "~/bin/" -*-
     ;;     Compilation started at Mon Jan 23 17:10:04
     ;;
     ;;     /usr/bin/perl /home/doom/bin/begoff TRASH!
     ;;     TRASH!
     ;;
     ;;     Compilation finished at Mon Jan 23 17:10:04

     (sleep-for 2) ;; wait for compile (only thing in emacs that's async)
     (set-buffer "*compilation*")
     (let* ( ( compilation-results (buffer-string) )
             ( trash-out-pat "^TRASH!")
             )
       (setq check-ok-p
             (assert-t
              (string-match trash-out-pat compilation-results)
              (concat "Testing that perlnow-run worked, and generated output")))
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
