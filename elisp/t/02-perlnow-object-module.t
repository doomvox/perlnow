#!/usr/local/bin/emacs --script
;; Or: /usr/bin/emacs

;;; perlnow-test.t ---

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

   (let* (
          (test-loc (test-init))
          (funcname "perlnow-object-module")
          (test-name (format "Testing %s" funcname ))

          (inc-spot perlnow-pm-location)
          (package-name "One::Over")
          (expected-file (concat inc-spot "One/Over.pm"))
          new-package-name
          )
     (perlnow-object-module inc-spot package-name)

     (assert-t
      (file-exists-p expected-file)
      (concat test-name ": creates expected file, " expected-file )
      )

     ;; now, look over the open code buffer
     (setq new-package-name (perlnow-get-package-name-from-module-buffer))

     (assert-equal package-name new-package-name
                   (concat test-name ":package line looks right"))

     (assert-t
      (not (perlnow-exporter-code-p))
      (concat test-name ": Looks like OOP (i.e. not Exporter)"))
     )))

(end-tests)


;;


;;========
;; LICENSE

;; This program is free software; you can redistribute it and/or modify
;; it under the same terms as the version of GNU Emacs you intend to use it with.

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
