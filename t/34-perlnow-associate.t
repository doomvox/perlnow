#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; /home/doom/End/Cave/Perlnow/lib/perlnow/elisp/t/34-perlnow-associate.t

;; The test story:

;; Create two perl buffers, directly using lower-level routines:
;;   perlnow-create-with-template
;;   perlnow-open-file-other-window
;; Associate the two with
;;   perlnow-set-associated-code-pointers
;; Check that each buffer has an appropriate value for
;;   perlnow-associated-code
;; Then repeat, but reversing the order of the arguments to:
;;   perlnow-set-associated-code-pointers

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
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))
          (test-name "Testing perlnow-set-associated-code-pointers, pm t")
          (pm-location perlnow-pm-location) ;; defined test-init-elisp.el
          (t-location
           (file-name-as-directory (concat pm-location "t")))
          (package-name "Gonzo")
          (pm-file (concat pm-location "Gonzo.pm"))
          (t-file  (concat t-location "Gonzo-weirdout.t"))
          (data-loc (concat default-directory "dat/"))
          (template-loc (concat data-loc "templates/"))
          (pm-template (concat template-loc "TEMPLATE.perlnow-pm.tpl"))
          (t-template  (concat template-loc "TEMPLATE.perlnow-pm-t.tpl"))

          pm-asscode  t-asscode
          )

     (require 'template)
     (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

     (test-init-move-file-out-of-way pm-file)
     (test-init-move-file-out-of-way t-file)

     (perlnow-create-with-template pm-file pm-template)
     ;; test that you now have a module file open
     (assert-t
      (perlnow-module-code-p)
      "Testing that we created a module")

     ;; (perlnow-open-file-other-window t-file 23 t-template t)
     (perlnow-open-file-other-window t-file 23 t-template)
     ;; test that this is a *.t file
     (assert-t
      (perlnow-test-p)
      "Testing that we created a test")
     (perlnow-set-associated-code-pointers pm-file t-file)
     (find-file pm-file)
     (setq pm-asscode perlnow-associated-code)
     (find-file t-file)
     (setq t-asscode perlnow-associated-code)
     ;; check that pm-asscode is t-file
     (assert-equal t-file pm-asscode
      (concat test-name ": pm has association with t") )

     ;; check that t-asscode is pm-file
     (assert-equal pm-file t-asscode
      (concat test-name ": t has association with pm") )
     )

   ;; New round of tests with reinitialization of everything, using
   ;; new names to avoid collisions with existing buffers, etx.
   (let* (
          (test-name "Testing perlnow-set-associated-code-pointers, t pm")
          (pm-location perlnow-pm-location) ;; defined test-init-elisp.el
          (t-location
           (file-name-as-directory (concat pm-location "t")))
          (package-name "BeatGen")
          (pm-file (concat pm-location "BeatGen.pm"))
          (t-file  (concat t-location "BeatGen-hangup.t"))
          (pm-template "/home/doom/.templates/TEMPLATE.perlnow-pm.tpl")
          (t-template  "/home/doom/.templates/TEMPLATE.perlnow-pm-t.tpl")
          pm-asscode  t-asscode
          )

     (require 'template)
     (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

     (test-init-move-file-out-of-way pm-file)
     (test-init-move-file-out-of-way t-file)

     (perlnow-create-with-template pm-file pm-template)
     ;; test that you now have a module file open
     (assert-t
      (perlnow-module-code-p)
      "Testing that we created a module")

     ;; (perlnow-open-file-other-window t-file 23 t-template t)
     (perlnow-open-file-other-window t-file 23 t-template)
     ;; test that this is a *.t file
     (assert-t
      (perlnow-test-p)
      "Testing that we created a test")
     (perlnow-set-associated-code-pointers pm-file t-file)
     (find-file pm-file)
     (setq pm-asscode perlnow-associated-code)
     (find-file t-file)
     (setq t-asscode perlnow-associated-code)
     ;; check that pm-asscode is t-file
     (assert-equal t-file pm-asscode
                   (concat test-name ": pm has association with t") )

     ;; check that t-asscode is pm-file
     (assert-equal pm-file t-asscode
                   (concat test-name ": t has association with pm"))
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
