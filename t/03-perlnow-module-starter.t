#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script

;;; 03-perlnow-module-starter.t

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   ;; project-specific include file (with standard name)
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)
   (let* (
          (test-loc (test-init))
          (funcname "perlnow-module-starter")
          (test-name
           (concat "Testing that " funcname " creates cpan-style module"))
          (package-name "Three::Warp::Nine")
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))
          (expected-file (concat staging-area "lib" perlnow-slash
                                 "Three" perlnow-slash "Warp" perlnow-slash "Nine.pm"))

          (expected-Pl (concat staging-area "Build.PL"))
          )
     ;; (test-init-safe-recursive-delete perlnow-dev-location)
     (perlnow-mkpath perlnow-dev-location)

     (test-init-safe-recursive-delete staging-area)
     (perlnow-mkpath staging-area)

     (perlnow-module-starter perlnow-dev-location package-name)

     (assert-t
      (file-exists-p expected-file)
      (concat test-name " " expected-file ) )

     (assert-t
      (file-exists-p expected-Pl)
      (concat "Testing that " funcname " generated expected Build.PL file" ) )
     )
   (end-tests)
   ))



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
