#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 52-t-dir-from-t.t

;; The test story:
;;
;; Just checking that this can find the "t" with different sub-dirs in the way:
;;   perlnow-t-dir-from-t
;;
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
          (funcname "perlnow-t-dir-from-t")
          (test-name (concat "Testing " funcname ))
          (slash test-init-slash)

          ;; "t" dir is put here, will test with different things tacked on
          (t-loc-loc
           (test-init-fixdir
              (concat "$HOME" slash "tmp" slash "devnasty1" slash)))

          ;; same expectations for all tests
          (expected-testloc-absolute
             (test-init-fixdir
              (concat
               t-loc-loc "t" slash
               )))
          testloc-absolute
          )
     ;; ACES! testfile: /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone.t
     ;;ACES! testloc-absolute: nil
     (let* ((test-case "t16 problem case: no intermediaries")
            (test-file-full-path
             "/home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone.t")
             (expected-testloc-absolute
              "/home/doom/tmp/perlnow_test/t16/t/")
             )
       ;; don't think I hit the file-system with this test, but you never know
       (test-init-move-file-out-of-way test-file-full-path)

       (setq testloc-absolute
             (perlnow-t-dir-from-t test-file-full-path))
       (assert-equal
        expected-testloc-absolute testloc-absolute
        (concat test-name ": " test-case))
       )
     (let* ((test-case "one intermediary layer")
            (test-file-full-path
             (test-init-fixdir "$HOME/tmp/devnasty1/t/intermediary/nice.t"))
            )
       ;; don't think I hit the file-system with this test, but you never know
       (test-init-move-file-out-of-way test-file-full-path)

       (setq testloc-absolute
             (perlnow-t-dir-from-t test-file-full-path))
       (assert-equal
        expected-testloc-absolute testloc-absolute
        (concat test-name ": " test-case))

       )
     (let* ((test-case "no intermediaries")
            ;; /home/doom/tmp/devnasty1/t/nice.t
            (test-file-full-path
              (concat
              t-loc-loc "t" slash "nice.t"
              ))
            )
       ;; don't think I hit the file-system with this test, but you never know
       (test-init-move-file-out-of-way test-file-full-path)
       (setq testloc-absolute
             (perlnow-t-dir-from-t test-file-full-path))
       (assert-equal
        expected-testloc-absolute testloc-absolute
        (concat test-name ": " test-case))
       )
     (let* ((test-case "two intermediary layers")
            (test-file-full-path
             (test-init-fixdir
              (concat
               t-loc-loc "t" slash "onesie" slash "twosie" slash "nice.t"
               )))
            )
       ;; don't think I hit the file-system with this test, but you never know
       (test-init-move-file-out-of-way test-file-full-path)

       (setq testloc-absolute
             (perlnow-t-dir-from-t test-file-full-path))
       (assert-equal
        expected-testloc-absolute testloc-absolute
        (concat test-name ": " test-case))

       )
     (let* ((test-case "no t level to find: nil")
            (test-file-full-path
             (test-init-fixdir
              (concat
               t-loc-loc "onesie" slash "twosie" slash "nice.t"
               )))

            )
       ;; don't think I hit the file-system with this test, but you never know
       (test-init-move-file-out-of-way test-file-full-path)

       (setq testloc-absolute
             (perlnow-t-dir-from-t test-file-full-path))
       (assert-nil
        testloc-absolute
        (concat test-name ": " test-case))
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
