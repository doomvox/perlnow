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

(if (file-exists-p "test-init-elisp.el")
    (load-file "test-init-elisp.el"))

(setenv "USE_TAP" "t")

;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
;; TODO
;;  install the latest, maybe via emacs package management: should have my fix.
(load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")

(test-simple-start) ;; Zero counters and start the stop watch.
;; (assert-t t "Checking assert-t")

 (setq perlnow-force t) ;; ask me no questions

(let* (
       (funcname "perlnow-module")
       (test-name
        (concat "Testing " funcname ))
       (package-name "Garbagio::Fantastico")
       (script-name "fill_can.pl")
       (expected-pm-base "Fantastico.pm")
       (expected-pm-file
        (concat perlnow-pm-location "Garbagio" perlnow-slash expected-pm-base))
       (expected-script
        (concat perlnow-script-location script-name))
       (sub-code-str
        "sub echo {my $arg=shift; print $arg ,\"\n\";}")
       (calling-code-str
        "echo($ARGV[0]);")
       (perl "/usr/bin/perl")
       )

  (perlnow-module perlnow-pm-location package-name)
  (insert sub-code-str)

  ;; Need to put the subname in EXPORT_TAGS list
  ;;  @ISA = qw(Exporter);
  ;;  %EXPORT_TAGS = ( 'all' => [
  ;;  # TODO Add names of items to export here.
  ;;  qw(

  (search-backward "%EXPORT_TAGS")
  (search-forward  "qw")
  (forward-char 2)
  (insert "echo")
  (save-buffer)

  ;; The pm file should exist on disk now.
  (assert-t
   (file-exists-p expected-pm-file)
   (concat test-name ": created file:\n   " expected-pm-file ) )

  (assert-t
    (perlnow-module-file-p (buffer-file-name))
    "Testing perlnow-module-file-p to confirm module looks like a module.")

  (assert-nil
    (perlnow-script-file-p (buffer-file-name))
    "Testing perlnow-script-file-p to confirm module is not like script.")

  ;; move a pre-existing script out of the way
  ;; (noninteractive call to perlnow-script doesn't deal with this well...)
  (let* ((filename expected-script)
         (backup-name (concat filename ".OLD")) )
   (cond ((file-exists-p filename)
          (message "07-*.t: renaming existing file: %s" filename)
          (rename-file filename backup-name t) ;; overwrites
          )))

  ;; (perlnow-script script-name)
  (perlnow-script expected-script)

  (insert calling-code-str)
  (save-buffer)

  ;; now the script file should exist on disk
  (assert-t
   (file-exists-p expected-script)
   (concat
    "testing that perlnow-script generated expected script:
    " script-name))

  (assert-t
    (perlnow-script-file-p (buffer-file-name))
    "Testing perlnow-script-file-p to confirm script looks like script.")

  (assert-nil
    (perlnow-module-file-p (buffer-file-name))
    "Testing perlnow-module-file-p to confirm script is not like module.")

  ;; TODO These need work to get closer to how you'd use them interactively
  (setq perlnow-run-string
        (concat perl " " expected-script " TRASH!"))
  (perlnow-run perlnow-run-string)

  ;; The compilation window show show the string "TRASH!" (in first column):
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
    ;; (message "compilation: %s" compilation-results) ;; DEBUG
    (setq check-ok-p
          (assert-t
           (string-match trash-out-pat compilation-results)
           (concat "Testing that perlnow-run worked, and generated output")))
    ) ;; end let*
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
