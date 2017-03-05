#!/usr/local/bin/emacs --script
;; 16-perlnow-metadata.t

;; Test stories:

;; (1) Create a module
;; Run perlnow-metadata.  Check return values.
;;     => done

;; TODO:

;; (2) Create a test from the module.
;; Run perlnow-metadata.  Check return values.  (( currently this would fail ))

;; (3) Go into the *select test file* menu
;; Run perlnow-metadata.  Check return values.


;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2017/02/26 17:38:51 doom Exp $
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

;; (defun sputtering-rutagbegas-of-doom ()
;;      ""

(funcall
  (lambda ()
       (let* (
              (funcname "perlnow-module")
              (test-name
               (concat "Testing " funcname ))
              (package-name "Beat::Skip")
              ;;       (script-name "scratch.pl")
              (expected-pm-base "Skip.pm")
              (expected-pm-file
               (concat perlnow-pm-location "Beat" perlnow-slash expected-pm-base))
              ;;       (expected-script
              ;;        (concat perlnow-script-location script-name))
              ;;       (sub-code-str
              ;;        "sub generate {my $arg=shift; print $arg ,\"\n\";}")
              ;;       (calling-code-str
              ;;        "echo($ARGV[0]);")
              (perl "/usr/bin/perl")
              )

         (perlnow-module perlnow-pm-location package-name)
         ;;  (insert sub-code-str)

         ;;   ;; Need to put the subname in EXPORT_TAGS list
         ;;   ;;  @ISA = qw(Exporter);
         ;;   ;;  %EXPORT_TAGS = ( 'all' => [
         ;;   ;;  # TODO Add names of items to export here.
         ;;   ;;  qw(

         ;;   (search-backward "%EXPORT_TAGS")
         ;;   (search-forward  "qw")
         ;;   (forward-char 2)
         ;;   (insert "echo")
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


         (let* (
                (funcname "perlnow-metadata")
                (test-name
                 (concat "Testing " funcname ))

                (md
                 (perlnow-metadata))
                (md-testloc          (nth 0  md))
                (md-dotdef           (nth 1  md))
                (md-namestyle        (nth 2  md))
                (md-testloc-absolute (nth 3  md))
                (md-hyphenized       (nth 4  md))
                (md-package-name     (nth 5  md))
                (md-incspot          (nth 6  md))
                (md-buffer           (nth 7  md))
                (md-filename         (nth 8  md))
                (md-fileloc          (nth 9  md))
                (md-basename         (nth 10 md))

                (expected-testloc "../t")
                (expected-dotdef "incspot")
                (expected-namestyle "fullauto")
                (expected-testloc-absolute "/home/doom/tmp/perlnow_test/t/")
                (expected-hyphenized "Beat-Skip")
                (expected-package-name "Beat::Skip")
                (expected-incspot "/home/doom/tmp/perlnow_test/lib/")
                (expected-filename "/home/doom/tmp/perlnow_test/lib/Beat/Skip.pm")
                (expected-fileloc "/home/doom/tmp/perlnow_test/lib/Beat/")
                (expected-basename "Skip")
                )

           (assert-equal expected-testloc md-testloc
            (format "Testing that %s got expected %s from module" funcname "testloc"))

           (assert-equal expected-dotdef md-dotdef
            (format "Testing that %s got expected %s from module" funcname "dotdef"))

           (assert-equal expected-namestyle md-namestyle
            (format "Testing that %s got expected %s from module" funcname "namestyle"))

           (assert-equal expected-testloc-absolute md-testloc-absolute
            (format "Testing that %s got expected %s from module" funcname "testloc-absolute"))

           (assert-equal expected-hyphenized md-hyphenized
            (format "Testing that %s got expected %s from module" funcname "hyphenized"))

           (assert-equal expected-package-name md-package-name
            (format "Testing that %s got expected %s from module" funcname "package-name"))

           (assert-equal expected-incspot md-incspot
            (format "Testing that %s got expected %s from module" funcname "incspot"))

           (assert-t (bufferp md-buffer)
            (format "Testing that %s got a buffer in field for buffer" funcname))

           (assert-equal expected-filename md-filename
            (format "Testing that %s got expected %s from module" funcname "filename"))

           (assert-equal expected-fileloc md-fileloc
            (format "Testing that %s got expected %s from module" funcname "fileloc"))

           (assert-equal expected-basename md-basename
            (format "Testing that %s got expected %s from module" funcname "basename"))

           ) ;; end let*
         ) ;; end let*
       ));; end lambda
;;       ) ;; end defun

;; (sputtering-rutagbegas-of-doom)

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
