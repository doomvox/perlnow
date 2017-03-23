#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 08-perlnow-script-from-cpan-style-module.t

;; Test story:

;; Create a cpan style perl module: perlnow-cpan-module
;; (effectively perlnow-milla).

;; Create a script from the module buffer: perlnow-script

;; Check location of the script: is it in a "script" subdir
;; inside the cpan staging area, next to "lib" and "t"?

;; Create a test for the script: perlnow-edit-test
;; Check location: is it in <staging area>/t?

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   (if (file-exists-p "test-init-elisp.el")
       (load-file "test-init-elisp.el"))

   ;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
   (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")
   (perlnow-tron)
   (let* (
          (test-loc (test-init))

          (funcname "perlnow-script")
          (test-name (concat "Testing " funcname ))
          (cpan-style   "milla")
          (test-ext-from  "") ;; pl, pm, or t
          (test-ext-new   "") ;; pl, pm, or t
          (test-mess      "") ;; for "new", from "from", in "context"
          (perl "/usr/bin/perl")
          (package-name "Borgia::BananaPeel")

          ;; pm: /home/doom/tmp/perlnow_test/dev/Osnome-Gnome/lib/Osnome/Gnome.pm
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))

          (exp-pm-base "BananaPeel.pm")
          (exp-pm-file
           (concat staging-area "lib"
                   perlnow-slash "Borgia" perlnow-slash exp-pm-base))

          ;; pl: /home/doom/tmp/perlnow_test/dev/Osnome-Gnome/script/deathskate.pl
          (script-name "deathskate.pl")
          ;; TODO  "bin" => "script"
          ;; (exp-script-file (concat staging-area "bin" perlnow-slash script-name))
          (exp-script-file (concat staging-area "script" perlnow-slash script-name))

          ;; t: /home/doom/tmp/perlnow_test/dev/Osnome-Gnome/t/01-Borgia-BananaPeel.t
          (t-mod-name "01-Borgia-BananaPeel.t")
          (exp-mod-t (concat staging-area "t" perlnow-slash t-mod-name))

          (t-script-name "02-Borgia-BananaPeel-deathskate-script.t") ;; TODO can do?
          (exp-t-loc (concat staging-area "t" perlnow-slash))
          (exp-script-t (concat exp-t-loc t-script-name))

          pm-file           pm-buffer
          t-file            t-buffer
          t-mod-file        t-mod-buffer
          t-script-file     t-script-buffer
          script-file       script-buffer

          (exp-asscode-for-script exp-pm-file)
          asscode-for-script

          (exp-asscode-for-mod exp-script-file)
          asscode-for-mod

          (exp-asscode-for-script-test exp-script-file)
          asscode-for-script-test

          )

     ;; For a new milla project,
     ;; get file names and buffers, first the module and test file
     ;; (along the way, do basic checks: verifying files were created as expected)

     (assert-equal cpan-style perlnow-cpan-style  ;; ok 1
       "Verifying that var perlnow-cpan-style is set to default, milla")
     ;; Alternately: (setq perlnow-cpan-style cpan-style)

     ;; runs perlnow-milla, but indirectly
     (perlnow-cpan-module perlnow-dev-location package-name)
     (setq pm-file   (buffer-file-name))
     (setq pm-buffer (current-buffer))

     (assert-t
      (perlnow-module-code-p)
      "Testing that perlnow-cpan-module left a pm buffer active.") ;; ok 2

     (assert-equal exp-pm-file pm-file
       "Testing that perlnow-cpan-module created expected pm file.") ;; ok 3

     (perlnow-back-to-code) ;; switch to associated *.t file
     (setq t-mod-file   (buffer-file-name))
     (setq t-mod-buffer (current-buffer))
     (assert-equal exp-mod-t t-mod-file
       (concat "Testing perlnow-cpan-module pm associated with test file")) ;; ok 4

     ;; now, a script generated from the module
     ;; script from pm
     (setq test-ext-from  "pm") ;; pl, pm, or t
     (setq test-ext-new   "pl") ;; pl, pm, or t
     (setq test-mess (concat test-ext-new " from " test-ext-from " in " cpan-style))

     ;; start from the pm, create script
     (set-buffer pm-buffer)

     ;; get rid of pre-existing script (non-interactive perlnow-script can't deal)
     (test-init-move-file-out-of-way exp-script-file)
     (perlnow-script exp-script-file)
     (setq script-file   (buffer-file-name))
     (setq script-buffer (current-buffer))

     ;; TODO check location of script first, before checking full path (?)
     (assert-equal exp-script-file script-file
       (concat test-name ": created expected script file from module, in 'script'")) ;; ok 5

     (setq asscode-for-script perlnow-associated-code)
     (assert-equal exp-asscode-for-script asscode-for-script
        (concat test-name ": associated code for script is the module")) ;; ok 6

     (set-buffer pm-buffer)
     (setq asscode-for-mod perlnow-associated-code)
     (assert-equal exp-asscode-for-mod asscode-for-mod
                   (concat test-name ": associated code for mod is the script"))    ;; ok 7

     (set-buffer script-buffer)
     (perlnow-edit-test-file)
     (setq t-script-file   (buffer-file-name))
     (setq t-script-buffer (current-buffer))

     (assert-equal
      exp-t-loc
      (perlnow-fixdir (file-name-directory t-script-file))
      (concat test-name
              ": test location for script is expected cpan t loc")) ;; ok 8

     (assert-equal exp-script-t t-script-file
        (concat test-name
           ": test file for script is named as expected, and in cpan t loc")) ;; ok 9

     (setq asscode-for-script-test perlnow-associated-code)
     (assert-equal exp-asscode-for-script-test asscode-for-script-test
                   (concat test-name ": associated code for script test is the script"))  ;; ok 10


     (set-buffer script-buffer)
     (setq exp-asscode-for-script t-script-file)

     (setq asscode-for-script perlnow-associated-code)
     (assert-equal exp-asscode-for-script asscode-for-script
        (concat test-name ": associated code for script has become script test"))      ;; ok 11

     (set-buffer pm-buffer)
     (setq asscode-for-mod perlnow-associated-code)
     (assert-equal exp-asscode-for-mod asscode-for-mod
                   (concat test-name ": associated code for mod remains the script"))  ;; ok 12

     (end-tests)
     )))


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
