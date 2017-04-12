#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 16-perlnow-metadata.t

;; Test stories:

;; (1) Create a module (exporter based)
;; Run perlnow-metadata.  Check return values.
;;     => done

;; TODO:

;; (2) Create a test from the module.
;; Run perlnow-metadata.  Check return values.

;; (3) Go into the *select test file* menu
;; Run perlnow-metadata.  Check return values.


;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2017/02/26 17:38:51 doom Exp $
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

          (funcname "perlnow-metadata")
          (test-name (concat "Testing " funcname ))
          (package-name "Beat::Skip")
          (expected-pm-base "Skip.pm")
          (expected-pm-file
           (concat perlnow-pm-location "Beat" perlnow-slash expected-pm-base))
          (perl "/usr/bin/perl")
          (expected-subname "blue_stone")

          sub-point  pm-buffer
          )

     (perlnow-module perlnow-pm-location package-name)
     (save-buffer)
     (setq pm-buffer (current-buffer))
     (perlnow-insert-sub expected-subname) ;; "blue_stone"
     (setq sub-point (point))

     ;; The pm file should exist on disk now.
     (assert-t
      (file-exists-p expected-pm-file)
      (concat "perlnow-module created file: \n  " expected-pm-file ))

     (assert-t
      (perlnow-module-file-p (buffer-file-name))
      "Testing perlnow-module-file-p to confirm module looks like a module.")

     (assert-nil
      (perlnow-script-file-p (buffer-file-name))
      "Testing perlnow-script-file-p to confirm module is not like script.")

     (if perlnow-debug
         (message "calling %s on MODULE~~~" funcname))

     (let* (
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
            (md-context          (nth 11 md))  ;; context:   module
            (md-subname          (nth 12 md))  ;; sub-name:  blue_stone

            (ignore0
             (message "md-context: >>>%s<<<<" md-context))

            (ignore1
             (message "md-subname: >>>%s<<<<" md-subname))

            (expected-testloc "../t")
            (expected-dotdef "incspot")
            (expected-namestyle "fullauto")
            ;; (expected-testloc-absolute "/home/doom/tmp/perlnow_test/t16/t/")
            (expected-testloc-absolute (concat test-loc "t" perlnow-slash))
            (expected-hyphenized "Beat-Skip")
            (expected-package-name "Beat::Skip")
            ;; (expected-incspot "/home/doom/tmp/perlnow_test/t16/lib/")
            (expected-incspot (concat test-loc "lib" perlnow-slash))
            ;; (expected-filename "/home/doom/tmp/perlnow_test/t16/lib/Beat/Skip.pm")
            (expected-lib expected-incspot)
            ;; (expected-fileloc "/home/doom/tmp/perlnow_test/t16/lib/Beat/")
            (expected-fileloc (concat expected-lib "Beat" perlnow-slash))
            (expected-filename (concat expected-fileloc "Skip.pm"))
            (expected-basename "Skip")
            (expected-context "module")
            )
       (assert-equal
        expected-testloc md-testloc
        (format "Testing that %s got expected %s from module" funcname "testloc"))

       (assert-equal
        expected-dotdef md-dotdef
        (format "Testing that %s got expected %s from module" funcname "dotdef"))

       (assert-equal
        expected-namestyle md-namestyle
        (format "Testing that %s got expected %s from module" funcname "namestyle"))

       (assert-equal
        expected-testloc-absolute md-testloc-absolute
        (format "Testing that %s got expected %s from module" funcname "testloc-absolute"))

       (assert-equal
        expected-hyphenized md-hyphenized
        (format "Testing that %s got expected %s from module" funcname "hyphenized"))

       (assert-equal
        expected-package-name md-package-name
        (format "Testing that %s got expected %s from module" funcname "package-name"))

       (assert-equal
        expected-incspot md-incspot
        (format "Testing that %s got expected %s from module" funcname "incspot"))

       (assert-t
        (bufferp md-buffer)
        (format "Testing that %s got a buffer in field for buffer" funcname))

       (assert-equal
        expected-filename md-filename
        (format "Testing that %s got expected %s from module" funcname "filename"))

       (assert-equal
        expected-fileloc md-fileloc
        (format "Testing that %s got expected %s from module" funcname "fileloc"))

       (assert-equal
        expected-basename md-basename
        (format "Testing that %s got expected %s from module" funcname "basename"))

       (assert-equal
        expected-context md-context
        (format "Testing that %s got expected %s from module" funcname "context"))

       (assert-equal
        expected-subname md-subname
        (format "Testing that %s got expected %s from module" funcname "subname"))
       )

     (switch-to-buffer pm-buffer)
     (goto-char sub-point)
     (perlnow-test-create)

     (if perlnow-debug
         (message "calling %s on TEST~~~" funcname))
     (let* (
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
            (md-context          (nth 11 md))
            (md-subname          (nth 12 md))

            (expected-testloc "../t")
            (expected-dotdef "incspot")
            (expected-namestyle "fullauto")
            (expected-testloc-absolute (concat test-loc "t" perlnow-slash))
            (expected-hyphenized "Beat-Skip")
            (expected-package-name "Beat::Skip")
            (expected-incspot (concat test-loc "lib" perlnow-slash))
            (expected-lib expected-incspot)
            ;; (expected-fileloc "/home/doom/tmp/perlnow_test/t16/t/")
            (expected-fileloc expected-testloc-absolute)
            (expected-filename (concat expected-fileloc "01-Beat-Skip-blue_stone.t"))
            (expected-basename "01-Beat-Skip-blue_stone")
            (expected-context "test")
            (expected-subname "blue_stone")
            )

    (if perlnow-debug
        (message
         (concat
          "   ~~~\n"
          (format "%30s %-40s\n" "testloc: " md-testloc)
          (format "%30s %-40s\n" "dotdef: " md-dotdef)
          (format "%30s %-40s\n" "namestyle: " md-namestyle)
          (format "%30s %-40s\n" "testloc-absolute: " md-testloc-absolute)
          (format "%30s %-40s\n" "hyphenized-package-name: " md-hyphenized)
          (format "%30s %-40s\n" "package-name: " md-package-name)
          (format "%30s %-40s\n" "incspot: " md-incspot)
          (format "%30s %-40s\n" "buffer: " (pp-to-string md-buffer))
          (format "%30s %-40s\n" "file-name: " md-filename)
          (format "%30s %-40s\n" "file-location: " md-fileloc)
          (format "%30s %-40s\n" "basename: " md-basename)
          (format "%30s %-40s\n" "context: " md-context)
          (format "%30s %-40s\n" "sub-name: " md-subname)
          "   ~~~\n"
          )))

;; Currently getting this:
;;                      testloc:  ../t
;;                       dotdef:  incspot
;;                    namestyle:  fullauto
;;             testloc-absolute:  nil
;;      hyphenized-package-name:  Beat-Skip-blue_stone        want: Beat-Skip
;;                 package-name:  Beat::Skip::blue_stone      want: Beat::Skip
;;                      incspot:  /home/doom/tmp/perlnow_test/t16/lib/
;;                       buffer:  #<buffer 01-Beat-Skip-blue_stone.t>
;;                    file-name:  /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone.t
;;                file-location:  /home/doom/tmp/perlnow_test/t16/t/
;;                     basename:  01-Beat-Skip-blue_stone
;;                      context:  test
;;                     sub-name:

       (assert-equal
        expected-testloc md-testloc
        (format "Testing that %s got expected %s from %s" funcname "testloc" expected-context))

       (assert-equal
        expected-dotdef md-dotdef
        (format "Testing that %s got expected %s from %s" funcname "dotdef" expected-context))

       (assert-equal
        expected-namestyle md-namestyle
        (format "Testing that %s got expected %s from %s" funcname "namestyle" expected-context))

       (assert-equal
        expected-testloc-absolute md-testloc-absolute
        (format "Testing that %s got expected %s from %s" funcname "testloc-absolute" expected-context))

       (assert-equal
        expected-hyphenized md-hyphenized
        (format "Testing that %s got expected %s from %s" funcname "hyphenized" expected-context))

       (assert-equal
        expected-package-name md-package-name
        (format "Testing that %s got expected %s from %s" funcname "package-name" expected-context))

       (assert-equal
        expected-incspot md-incspot
        (format "Testing that %s got expected %s from %s" funcname "%s" expected-context))

       (assert-t
        (bufferp md-buffer)
        (format "Testing that %s got a buffer in field for buffer" funcname))

       (assert-equal
        expected-filename md-filename
        (format "Testing that %s got expected %s from %s" funcname "filename" expected-context))

       (assert-equal
        expected-fileloc md-fileloc
        (format "Testing that %s got expected %s from %s" funcname "fileloc" expected-context))

       (assert-equal
        expected-basename md-basename
        (format "Testing that %s got expected %s from %s" funcname "basename" expected-context))

       (assert-equal
        expected-context md-context
        (format "Testing that %s got expected %s from %s" funcname "context" expected-context))

       (assert-equal
        expected-subname md-subname
        (format "Testing that %s got expected %s from %s" funcname "subname" expected-context))

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
