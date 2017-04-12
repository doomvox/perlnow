#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script

;; 50-perlnow-test-p.t

;; Test story:

;; Have an existing test file (of an object module):
;;   ~/End/Cave/Perlnow/lib/perlnow/elisp/t/dat/code/s50/t/01-Stringp-Umbrella.t

;; Also have the related module file:
;;   ~/End/Cave/Perlnow/lib/perlnow/elisp/t/dat/code/s50/lib/Stringp/Umbrella.pm

;; Run perlnow-test-p on both: it should identify one and not the other.

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
   (let* (
          (test-loc (test-init)) ;; we'll use a lib and t in there
          (test-name "Testing perlnow-test-p")
     ;;    case-name
          (slash test-init-slash) ;; on unix, "/"

          (package-name "Stringp::Umbrella")
          (pm-base "Umbrella")
          (t-name "01-Stringp-Umbrella.t")

          (data-loc (concat default-directory "dat" slash))
          (source-loc (concat data-loc "code" slash "s50" slash))

          (source-t   (concat source-loc "t"   slash "01-Stringp-Umbrella.t"))
          (source-pm  (concat source-loc "lib" slash "Stringp" slash "Umbrella.pm"))

          (pm-loc  (concat perlnow-pm-location "Stringp" perlnow-slash))
          (pm-file (concat pm-loc  pm-base ".pm"))
          (t-loc   (test-init-fixdir (concat perlnow-pm-location slash ".." slash "t")))
          (t-file  (concat t-loc t-name))

          pm-buffer t-buffer ret
          )


     ;; Initialize the t50 tree with t and module in dat/code
     (test-init-mkpath t-loc)
     (copy-file source-t t-loc t)
     (test-init-mkpath pm-loc)
     (copy-file source-pm pm-loc t)

     ;; open the test file
     (cond ((file-exists-p t-file)
            (find-file t-file)
            (setq t-buffer (current-buffer))
            )
           (t
            (message "Can't find t-file: %s" (pp-to-string t-file))
            ))

     (setq ret
           (perlnow-test-p))

     (assert-t ret (concat test-name ": sees test file as a test"))

     ;; open the module file
     (cond ((file-exists-p pm-file)
            (find-file pm-file)
            (setq pm-buffer (current-buffer))
            )
           (t
            (message "Can't find pm-file: %s" (pp-to-string pm-file))
            ))

     (setq ret
           (perlnow-test-p))

     (assert-nil ret (concat test-name ": does not see module as a test"))
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

;;; 40-perlnow-from-pl-nexterr-to-pm-then-run.t  ends here
