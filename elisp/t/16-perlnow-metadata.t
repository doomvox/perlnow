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
          (slash perlnow-slash)
          (package-name "Beat::Skip")
          (expected-pm-base "Skip.pm")
          (expected-pm-file
           (concat perlnow-pm-location "Beat" slash expected-pm-base))
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
      (concat "perlnow-module created file: " expected-pm-file ))

     (assert-t
      (perlnow-module-file-p (buffer-file-name))
      "Testing perlnow-module-file-p to confirm module looks like a module.")

     (assert-nil
      (perlnow-script-file-p (buffer-file-name))
      "Testing perlnow-script-file-p to confirm module is not like script.")

     (if perlnow-debug
         (message "calling %s on file-type MODULE" funcname))

     (let* ((md (perlnow-metadata))
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
            (md-file-type        (nth 11 md))  ;; context:   module
            (md-project-type     (nth 12 md))
            (md-subname          (nth 13 md))  ;; sub-name:  blue_stone
            (expected-testloc "../t")
            (expected-dotdef "incspot")
            (expected-namestyle "fullauto")
            ;; (expected-testloc-absolute "/home/doom/tmp/perlnow_test/t16/t/")
            (expected-testloc-absolute (concat test-loc "t" slash))
            (expected-hyphenized   "Beat-Skip")
            (expected-package-name "Beat::Skip")
            ;; (expected-incspot "/home/doom/tmp/perlnow_test/t16/lib/")
            (expected-incspot (concat test-loc "lib" slash))
            ;; (expected-filename "/home/doom/tmp/perlnow_test/t16/lib/Beat/Skip.pm")
            (expected-lib expected-incspot)
            ;; (expected-fileloc "/home/doom/tmp/perlnow_test/t16/lib/Beat/")
            (expected-fileloc (concat expected-lib "Beat" slash))
            (expected-filename (concat expected-fileloc "Skip.pm"))
            (expected-basename "Skip")
            (expected-file-type "module")
            (expected-project-type "noncpan")
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
        expected-file-type md-file-type
        (format "Testing that %s got expected %s from module" funcname "md-file-type"))

       (assert-equal
        expected-project-type md-project-type
        (format "Testing that %s got expected %s from module" funcname "md-project-type"))

       (assert-equal
        expected-subname md-subname
        (format "Testing that %s got expected %s from module" funcname "subname"))
       )
     (perlnow-tron)
     (switch-to-buffer pm-buffer)
     (goto-char sub-point)
     (perlnow-test-create)

     (setq perlnow-debug t) ;; TODO why would I need to do this here?

     (if perlnow-debug
         (message "calling %s on file-type TEST" funcname))

     (let* ((md (perlnow-metadata))
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
            (md-file-type        (nth 11 md))
            (md-project-type     (nth 12 md))
            (md-subname          (nth 13 md))
            (expected-testloc     "../t")
            (expected-dotdef      "incspot")
            (expected-namestyle   "fullauto")
            (expected-testloc-absolute (concat test-loc "t" slash))
            (expected-hyphenized   "Beat-Skip")
            (expected-package-name "Beat::Skip")
            (expected-incspot (concat test-loc "lib" slash))
            (expected-lib expected-incspot)
            ;; (expected-fileloc "/home/doom/tmp/perlnow_test/t16/t/")
            (expected-fileloc expected-testloc-absolute)
            (expected-filename (concat expected-fileloc "01-Beat-Skip-blue_stone.t"))
            (expected-basename     "01-Beat-Skip-blue_stone")
            (expected-file-type    "test")
            (expected-project-type "noncpan")
            (expected-subname      "blue_stone")
            )

    (setq perlnow-debug t) ;; TODO What keeps resetting this?
    (if perlnow-debug
      (perlnow-report-metadata md))

       (assert-equal
        expected-testloc md-testloc
        (format "Testing that %s got expected %s from %s" funcname "testloc" expected-file-type))

       (assert-equal
        expected-dotdef md-dotdef
        (format "Testing that %s got expected %s from %s" funcname "dotdef" expected-file-type))

       (assert-equal
        expected-namestyle md-namestyle
        (format "Testing that %s got expected %s from %s" funcname "namestyle" expected-file-type))

       (assert-equal
        expected-testloc-absolute md-testloc-absolute
        (format "Testing that %s got expected %s from %s" funcname "testloc-absolute" expected-file-type))

       (assert-equal
        expected-hyphenized md-hyphenized
        (format "Testing that %s got expected %s from %s" funcname "hyphenized" expected-file-type))

       (assert-equal
        expected-package-name md-package-name
        (format "Testing that %s got expected %s from %s" funcname "package-name" expected-file-type))

       (assert-equal
        expected-incspot md-incspot
        (format "Testing that %s got expected %s from %s" funcname "%s" expected-file-type))

       (assert-t
        (bufferp md-buffer)
        (format "Testing that %s got a buffer in field for buffer" funcname))

       (assert-equal
        expected-filename md-filename
        (format "Testing that %s got expected %s from %s" funcname "filename" expected-file-type))

       (assert-equal
        expected-fileloc md-fileloc
        (format "Testing that %s got expected %s from %s" funcname "fileloc" expected-file-type))

       (assert-equal
        expected-basename md-basename
        (format "Testing that %s got expected %s from %s" funcname "basename" expected-file-type))

       (assert-equal
        expected-file-type md-file-type
        (format "Testing that %s got expected %s from %s" funcname "context" expected-file-type))

       (assert-equal
        expected-project-type md-project-type
        (format "Testing that %s got expected %s from module" funcname "md-project-type"))

       (assert-equal
        expected-subname md-subname
        (format "Testing that %s got expected %s from %s" funcname "subname" expected-file-type))
       ) ;; end let* perlnow-metadata on TEST


     (let*(
           (expected-package-name "Interociter::Comedian")
           (expected-subname "know_her")
           )
       (perlnow-milla perlnow-dev-location expected-package-name)
       (perlnow-insert-sub expected-subname)
       (insert "return 0;")
       (save-buffer)

       (setq perlnow-debug t) ;; TODO why would I need to do this here?

       (if perlnow-debug
           (message "calling %s on file-type OBJECT, project-type CPAN" funcname))

       (let* ((md (perlnow-metadata))
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
              (md-file-type        (nth 11 md))
              (md-project-type     (nth 12 md))
              (md-subname          (nth 13 md))
              (expected-testloc     "../t")
              (expected-dotdef      "incspot")
              (expected-namestyle   "fullauto")

              (expected-hyphenized   "Interociter-Comedian")
              (expected-package-name "Interociter::Comedian")


              (expected-staging-area
               (concat perlnow-dev-location expected-hyphenized slash))



              (expected-incspot (concat expected-staging-area "lib" slash))

              (expected-testloc-absolute (concat expected-staging-area "t" slash))

              (expected-fileloc (concat expected-incspot "Interociter" slash))

              (expected-filename (concat expected-fileloc "Comedian.pm"))
              (expected-basename     "Comedian")
              (expected-file-type    "object")
              (expected-project-type "cpan")
              )

         (setq perlnow-debug t) ;; TODO What keeps resetting this?
         (if perlnow-debug
             (perlnow-report-metadata md))

         (assert-equal
          expected-testloc md-testloc
          (format "Testing that %s got expected %s from %s" funcname "testloc" expected-file-type))

         (assert-equal
          expected-dotdef md-dotdef
          (format "Testing that %s got expected %s from %s" funcname "dotdef" expected-file-type))

         (assert-equal
          expected-namestyle md-namestyle
          (format "Testing that %s got expected %s from %s" funcname "namestyle" expected-file-type))

         (assert-equal
          expected-testloc-absolute md-testloc-absolute
          (format "Testing that %s got expected %s from %s" funcname "testloc-absolute" expected-file-type))

         (assert-equal
          expected-hyphenized md-hyphenized
          (format "Testing that %s got expected %s from %s" funcname "hyphenized" expected-file-type))

         (assert-equal
          expected-package-name md-package-name
          (format "Testing that %s got expected %s from %s" funcname "package-name" expected-file-type))

         (assert-equal
          expected-incspot md-incspot
          (format "Testing that %s got expected %s from %s" funcname "%s" expected-file-type))

         (assert-t
          (bufferp md-buffer)
          (format "Testing that %s got a buffer in field for buffer" funcname))

         (assert-equal
          expected-filename md-filename
          (format "Testing that %s got expected %s from %s" funcname "filename" expected-file-type))

         (assert-equal
          expected-fileloc md-fileloc
          (format "Testing that %s got expected %s from %s" funcname "fileloc" expected-file-type))

         (assert-equal
          expected-basename md-basename
          (format "Testing that %s got expected %s from %s" funcname "basename" expected-file-type))

         (assert-equal
          expected-file-type md-file-type
          (format "Testing that %s got expected %s from %s" funcname "context" expected-file-type))

         (assert-equal
          expected-project-type md-project-type
          (format "Testing that %s got expected %s from module" funcname "md-project-type"))

         (assert-equal
          expected-subname md-subname
          (format "Testing that %s got expected %s from %s" funcname "subname" expected-file-type))
         ))





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
