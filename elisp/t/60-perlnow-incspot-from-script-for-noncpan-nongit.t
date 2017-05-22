#! /usr/bin/emacs --script
;; 60-perlnow-incspot-from-script-for-noncpan-nongit.t


;; Test story.

;;   case: bin & lib in same place
;;   Create a non-cpan module.
;;          <blah>/lib/Pelucidar::Float
;;   Create a script from the module.
;;          <blah>/bin/armpflap.pl
;;   perlnow-incspot-from-script-for-noncpan-nongit

;;   case: lib one down from location of bin
;;   Create a non-cpan module.
;;          <blah>/lib/projectile/Kryptonite::Shuffle
;;   Create a script from the module.
;;          <blah>/bin/redrum.pl
;;   perlnow-incspot-from-script-for-noncpan-nongit

;;   case: bin one down from location of lib
;;   Create a non-cpan module.
;;          <blah>/lib/Osnome::Gnome
;;   Create a script from the module.
;;          <blah>/bin/gnome/greener.pl
;;   perlnow-incspot-from-script-for-noncpan-nongit


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

   (let* ((test-loc (test-init))
          (test-name "")
          (slash perlnow-slash)
          )
     ;; case: bin & lib in same place
     ;;   Create a non-cpan module.
     ;;          <blah>/lib/Pelucidar::Float
     ;;   Create a script from the module.
     ;;          <blah>/bin/armpflap.pl
     ;;   perlnow-incspot-from-script-for-noncpan-nongit
     (let* (
            (package-name "Pelucidar::Float")
            (script-name "armflap.pl")
            (expected-pm-base "Float.pm")

            ;;          (lib-loc (concat perlnow-pm-location     "lib" slash))
            (lib-loc perlnow-pm-location)
            ;;          (bin-loc (concat perlnow-script-location "bin" slash))
            (bin-loc perlnow-script-location)

            (expected-pm-file
             (concat lib-loc "Pelucidar" slash expected-pm-base))
            (expected-script
             (concat bin-loc script-name))

            pm-buffer  pm-file  script-buffer  determined-incspot
            )
       ;; TODO can use this as start point of alternate bin/lib locs
       (message "@##@ test-loc: %s" test-loc) ;; @##@ test-loc: /home/doom/tmp/perlnow_test/t60/
       (setq test-name "Testing perlnow-module")
       (if perlnow-debug (message "RIDEON: %s" test-name))
       (perlnow-module perlnow-pm-location package-name)
       (save-buffer)
       (setq pm-buffer (current-buffer))
       (setq pm-file (buffer-file-name))

       ;; Note: the following four tests are redundant with, e.g. 07-*.t
       ;; The pm file should exist on disk now.
       (assert-t
        (file-exists-p expected-pm-file)
        (concat test-name ": created file:\n       " expected-pm-file )) ;; ok 1

       (assert-t
        (perlnow-module-file-p (buffer-file-name))
        "Testing perlnow-module-file-p to confirm module looks like a module.") ;; ok 2

       (assert-nil
        (perlnow-script-file-p (buffer-file-name))
        "Testing perlnow-script-file-p to confirm module is not like script.") ;; ok 3

       (assert-equal
        expected-pm-file pm-file
        "Testing perlnow-module: confirm created file is named as expected") ;; ok 4

       ;; move a pre-existing script out of the way
       ;; (noninteractive call to perlnow-script doesn't deal with this well...)
       (test-init-move-file-out-of-way expected-script)

       (setq test-name "Testing perlnow-script")
       (set-buffer pm-buffer)
       (perlnow-script expected-script)
       (save-buffer)

       ;; now the script file should exist on disk
       (assert-t
        (file-exists-p expected-script)
        (concat test-name ": generated expected script: " script-name))

       (assert-t
        (perlnow-script-file-p (buffer-file-name))
        "Testing script looks like script via perlnow-script-file-p.")

       (assert-nil
        (perlnow-module-file-p (buffer-file-name))
        "Testing script is not like module via perlnow-module-file-p.")

       (message "BFD, bfn: %s" (buffer-file-name))
       (message "SEY: perlnow-vars-report-string\n")
       (message (perlnow-vars-report-string))

       (setq test-name "Testing perlnow-incspot-from-script-for-noncpan-nongit")
       (setq determined-incspot (perlnow-incspot-from-script-for-noncpan-nongit))

       (assert-equal
        lib-loc determined-incspot
        (concat test-name
                (format ": found incspot: %s from script loc %s " determined-incspot bin-loc)))
       )
     ;;   case: lib one down from location of binq
     ;;   Create a non-cpan module.
     ;;          <blah>/lib/projectile/Kryptonite::Shuffle
     ;;   Create a script from the module.
     ;;          <blah>/bin/redrum.pl
     ;;   perlnow-incspot-from-script-for-noncpan-nongit
     (let* (
            (package-name "Kryptonite::Shuffle")
            (script-name "redrum.pl")
            (expected-pm-base "Shuffle.pm")
            ;; test-loc: /home/doom/tmp/perlnow_test/t60/
            (alt-test-loc (test-init-fixdir (concat test-loc ".." slash "t60b")))
            (lib-loc (concat alt-test-loc "projectile" slash "lib" slash))
            (bin-loc (concat alt-test-loc slash "bin" slash))
            (perlnow-script-location bin-loc)
            (perlnow-pm-location     lib-loc)

            (expected-pm-file
             (concat lib-loc "Kryptonite" slash expected-pm-base))
            (expected-script
             (concat bin-loc script-name))

            pm-buffer  pm-file  script-buffer  determined-incspot
            )

       (test-init-mkpath alt-test-loc)
       (test-init-mkpath lib-loc)
       (test-init-mkpath bin-loc)

       (perlnow-module perlnow-pm-location package-name)
       (save-buffer)
       (setq pm-buffer (current-buffer))
       (setq pm-file (buffer-file-name))

       ;; The pm file should exist on disk now.
       (assert-t
        (file-exists-p expected-pm-file)
        (concat test-name ": created file:\n       " expected-pm-file )) ;; ok N

       (assert-t
        (perlnow-module-file-p (buffer-file-name))
        "Testing perlnow-module-file-p to confirm module looks like a module.") ;; ok N

       (assert-nil
        (perlnow-script-file-p (buffer-file-name))
        "Testing perlnow-script-file-p to confirm module is not like script.") ;; ok N

       (assert-equal
        expected-pm-file pm-file
        "Testing perlnow-module: confirm created file is named as expected") ;; ok N

       ;; move a pre-existing script out of the way
       ;; (noninteractive call to perlnow-script doesn't deal with this well...)
       (test-init-move-file-out-of-way expected-script)

       (setq test-name "Testing perlnow-script")
       (set-buffer pm-buffer)
       (perlnow-script expected-script)
       (save-buffer)

       ;; now the script file should exist on disk
       (assert-t
        (file-exists-p expected-script)
        (concat test-name ": generated expected script: " script-name))

       (assert-t
        (perlnow-script-file-p (buffer-file-name))
        "Testing script looks like script via perlnow-script-file-p.")

       (assert-nil
        (perlnow-module-file-p (buffer-file-name))
        "Testing script is not like module via perlnow-module-file-p.")

       (message "SCRANTON: bfn: %s" (buffer-file-name))
;;       (message "SCANTRON: perlnow-vars-report-string\n")
       (message (perlnow-vars-report-string))

       (setq test-name "Testing perlnow-incspot-from-script-for-noncpan-nongit")
       (setq determined-incspot (perlnow-incspot-from-script-for-noncpan-nongit))

       (assert-equal
        lib-loc determined-incspot
        (concat test-name
                (format ": found incspot: %s from script loc %s " determined-incspot bin-loc)))
       )
     ;;   case: bin one down from location of lib
     ;;   Create a non-cpan module.
     ;;          <blah>/lib/Osnome::Gnome
     ;;   Create a script from the module.
     ;;          <blah>/bin/gnome/greener.pl
     ;;   perlnow-incspot-from-script-for-noncpan-nongit
     (let* (
            (package-name "Osnome::Gnome")
            (script-name "greener.pl")
            (expected-pm-base "Gnome.pm")
            ;; test-loc: /home/doom/tmp/perlnow_test/t60/
            (alt-test-loc (test-init-fixdir (concat test-loc ".." slash "t60c")))
            (lib-loc (concat alt-test-loc "lib" slash))
            (bin-loc (concat alt-test-loc slash "bin" slash "gnome" slash))
            (perlnow-script-location bin-loc)
            (perlnow-pm-location     lib-loc)

            (expected-pm-file
             (concat lib-loc "Osnome" slash expected-pm-base))
            (expected-script
             (concat bin-loc script-name))

            pm-buffer  pm-file  script-buffer  determined-incspot
            )

       (test-init-mkpath alt-test-loc)
       (test-init-mkpath lib-loc)
       (test-init-mkpath bin-loc)

       (perlnow-module perlnow-pm-location package-name)
       (save-buffer)
       (setq pm-buffer (current-buffer))
       (setq pm-file (buffer-file-name))

       ;; The pm file should exist on disk now.
       (assert-t
        (file-exists-p expected-pm-file)
        (concat test-name ": created file:\n       " expected-pm-file )) ;; ok N

       (assert-t
        (perlnow-module-file-p (buffer-file-name))
        "Testing perlnow-module-file-p to confirm module looks like a module.") ;; ok N

       (assert-nil
        (perlnow-script-file-p (buffer-file-name))
        "Testing perlnow-script-file-p to confirm module is not like script.") ;; ok N

       (assert-equal
        expected-pm-file pm-file
        "Testing perlnow-module: confirm created file is named as expected") ;; ok N

       ;; move a pre-existing script out of the way
       ;; (noninteractive call to perlnow-script doesn't deal with this well...)
       (test-init-move-file-out-of-way expected-script)

       (setq test-name "Testing perlnow-script")
       (set-buffer pm-buffer)
       (perlnow-script expected-script)
       (save-buffer)

       ;; now the script file should exist on disk
       (assert-t
        (file-exists-p expected-script)
        (concat test-name ": generated expected script: " script-name))

       (assert-t
        (perlnow-script-file-p (buffer-file-name))
        "Testing script looks like script via perlnow-script-file-p.")

       (assert-nil
        (perlnow-module-file-p (buffer-file-name))
        "Testing script is not like module via perlnow-module-file-p.")

       (message "SCRANTON: bfn: %s" (buffer-file-name))
       ;;       (message "SCANTRON: perlnow-vars-report-string\n")
       (message (perlnow-vars-report-string))

       (setq test-name "Testing perlnow-incspot-from-script-for-noncpan-nongit")
       (setq determined-incspot (perlnow-incspot-from-script-for-noncpan-nongit))

       (assert-equal
        lib-loc determined-incspot
        (concat test-name
                (format ": found incspot: %s from script loc %s " determined-incspot bin-loc)))
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
