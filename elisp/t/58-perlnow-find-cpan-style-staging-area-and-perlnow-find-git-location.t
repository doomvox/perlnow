#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 58-perlnow-find-cpan-style-staging-area-and-perlnow-find-git-location.t

;; Test story:

;; Create a cpan style structure like so:
;;     perlnow-milla
;; Add a few scripts, a few tests, another module or two...

;; Starting from various files in the tree, this should get the same loc:

;;   (setq staging-area
;;     (perlnow-find-cpan-style-staging-area file-name ))

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
          (funcname "perlnow-find-cpan-style-staging-area")
          (test-name (concat "Testing " funcname ))
          (slash perlnow-slash)
          (mod-style   "milla") ;; noncpan, milla, modstar, h2xs...
          ;; milla (with Module::Build)
          (package-name "Peruvian::Banana::Stain")

          ;; pm: /home/doom/tmp/perlnow_test/dev/Peruvian-Banana-Stain/lib/Peruvian/Banana/Stain.pm
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))

          (exp-pm-file
           (concat staging-area "lib"
              slash "Peruvian" slash "Banana" slash "Stain" ".pm"))

          ;; pl: /home/doom/tmp/perlnow_test/dev/Peruvian-Banana-Stain/bin/skintoss.pl
          (script-name "skintoss.pl")
          (exp-script (concat staging-area "bin" perlnow-slash script-name))

          ;; t: /home/doom/tmp/perlnow_test/dev/Osnome-Gnome/
          ;;    /home/doom/tmp/perlnow_test/dev/Peruvian-Banana-Stain/t/01-Peruvian-Banana-Stain.t
          (t-name "01-Peruvian-Banana-Stain.t")
          (exp-t (concat staging-area "t" perlnow-slash t-name))

          pm-file          pm-buffer
          t-file           t-buffer
          script-file      script-buffer

          st-from-script
          st-from-t
          st-from-pm
          )
     (if perlnow-debug (message (upcase mod-style)))

     ;; For a new milla project,
     ;; get file names and buffers, first the module and test file
     ;; (along the way, do basic checks: verifying files were created as expected)
     (perlnow-milla perlnow-dev-location package-name)
     (setq pm-file   (buffer-file-name))
     (setq pm-buffer (current-buffer))

     (assert-t
      (perlnow-module-code-p)
      "Testing that perlnow-milla left new pm buffer active.") ;; ok 1

     (perlnow-back-to-code) ;; switch to associated *.t file
     (setq t-file   (buffer-file-name))
     (setq t-buffer (current-buffer))

     (if perlnow-debug (message "t created by %s run: %s \nexpected: %s" mod-style t-file exp-t))
     (assert-equal exp-t t-file
                   (concat "Testing milla run created test file: \n"
                           "        " t-file )) ;; ok 2

     ;; now, a script generated from the module
     ;;   script from pm
     (set-buffer pm-buffer)
     ;; get rid of pre-existing script (non-interactive perlnow-script can't deal)
     (test-init-move-file-out-of-way exp-script)
     (perlnow-script exp-script)
     (setq script-file   (buffer-file-name))
     (setq script-buffer (current-buffer))
     (assert-equal exp-script script-file
                   (format "Testing perlnow-script in %s: \n %s" mod-style script-file)) ;; ok 4

    (setq st-from-pm
      (perlnow-find-cpan-style-staging-area pm-file))

    (assert-equal staging-area st-from-pm
       (format "Testing %s from %s" funcname "module"))

    (setq st-from-script
      (perlnow-find-cpan-style-staging-area script-file))

    (assert-equal staging-area st-from-script
       (format "Testing %s from %s" funcname "script"))

    (setq st-from-t
      (perlnow-find-cpan-style-staging-area t-file))

    (assert-equal staging-area st-from-t
       (format "Testing %s from %s" funcname "test"))

    (let* ((funcname "perlnow-find-git-location")
           )

      (setq st-from-pm
            (perlnow-find-git-location pm-file))

      (assert-equal staging-area st-from-pm
                    (format "Testing %s from %s" funcname "module"))

      (setq st-from-script
            (perlnow-find-git-location script-file))

      (assert-equal staging-area st-from-script
                    (format "Testing %s from %s" funcname "script"))

      (setq st-from-t
            (perlnow-find-git-location t-file))

      (assert-equal staging-area st-from-t
                    (format "Testing %s from %s" funcname "test"))

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
