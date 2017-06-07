#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 24-perlnow-next-test-prefix.t

;; The test story:
;;
;; Create a t directory, populate with multiple NN-*.t files.
;; See if perlnow-next-test-prefix returns NN+1.

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

          (perlnow-misc-location
                (file-name-as-directory (concat test-loc "misc")))
          (perlnow-mkpath perlnow-misc-location)
          (t-loc
           (file-name-as-directory (concat perlnow-misc-location "t")))
          (funcname "perlnow-next-test-prefix")
          (test-name (concat "Testing " funcname ))
          (t-list (list "01-yo.t" "69-up.t" "03-ho.t" "27-out.t"))
          (expected "70")
          ret
          )
    (perlnow-mkpath t-loc)

    (dolist (file t-list)
      (let ( (cmd
              (concat "touch " t-loc file)))
        (call-process-shell-command cmd)
      ))
    (setq ret
          (perlnow-next-test-prefix t-loc))
    (assert-equal ret expected
     (concat test-name ": returned max existing prefix plus one") )
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
