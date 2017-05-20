#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 48-perlnow-follow-associations-to-non-test-code.t

;; Test story:

;; Create a module.
;; Create a script using the module.
;; Run:
;;   perlnow-follow-associations-to-non-test-code
;; Are we at the module?

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
   (message "~ ~ ~") ;; DEBUG
   (message "AAA")   ;; DEBUG
   (let* (
          (test-loc (test-init))
          (test-name "Testing perlnow-follow-associations-to-non-test-code, mod to script")
          (package-name "Mirror::Diagnostics")
          (script-name "fairest.pl")
          (script-base "fairest")
          (expected-pm-base "Diagnostics")
          (expected-pm-file
            (concat perlnow-pm-location "Mirror" perlnow-slash expected-pm-base ".pm"))
          (expected-script
           (concat perlnow-script-location script-name))
          pm-buffer script-buffer that-car
          )
     (perlnow-module perlnow-pm-location package-name)
     (save-buffer)
     (setq pm-buffer (current-buffer))

     (test-init-move-file-out-of-way expected-script)
     (perlnow-script expected-script)
     (setq script-buffer (current-buffer))
     (save-buffer)

     ;; (message "stat vars: %s" (perlnow-vars-report-string))

     (setq that-car
           (perlnow-follow-associations-to-non-test-code))

     (assert-equal expected-pm-file that-car
                   (concat test-name ": back at module"))

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
