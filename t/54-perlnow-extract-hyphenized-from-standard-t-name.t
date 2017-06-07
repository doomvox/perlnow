#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 54-perlnow-extract-hyphenized-from-standard-t-name.t

;; The test story:
;;
;; Make sure this isn't confused by a sub-name embedded in *.t name.
;;   perlnow-extract-hyphenized-from-standard-t-name
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
          (funcname "perlnow-parse-standard-t-name")
          (test-name (concat "Testing " funcname ))

          (slash test-init-slash)

          ;; path we'll use in (most) test cases
          (t-loc
           (test-init-fixdir
            (concat "$HOME" slash "tmp" slash "perlnow_test" slash "t54" slash "t")))
          )

;; Going to get rid of this routine, I think, so zapping the test
;;           (funcname "perlnow-extract-hyphenized-from-standard-t-name")
;;           (test-name (concat "Testing " funcname ))

;;      (let* ((test-case "t16 problem case: following sub-name appended to hyphenized")
;;             (test-file-full-path ;; TODO use slash var, etc.
;;              "/home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone.t")
;;              (expected-hyphenized
;;               "Beat-Skip")
;;              )

;;        (setq hyphenized
;;              (perlnow-extract-hyphenized-from-standard-t-name test-file-full-path))
;;        (assert-equal
;;         expected-hyphenized hyphenized
;;         (concat test-name ": " test-case))
;;        )
     (let* ((test-case "01-Beat-Skip-blue_stone")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      "01")
            (expected-hyphenized  "Beat-Skip")
            (expected-subname     "blue_stone")
            (expected-description nil)
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
       )
     (let* ((test-case "01-Beat-Skip-blue_stone-magical")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone-magical.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      "01")
            (expected-hyphenized  "Beat-Skip")
            (expected-subname     "blue_stone")
            (expected-description "magical")
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
       )
     (let* ((test-case "66-Beat-Skip")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone-magical.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      "66")
            (expected-hyphenized  "Beat-Skip")
            (expected-subname     nil)
            (expected-description nil)
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
       )

     (let* ((test-case "09-Rooty-Toot-Baby-flush_loud-echoy_enough-xtra_field")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone-magical.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      "09")
            (expected-hyphenized  "Rooty-Toot-Baby")
            (expected-subname     "flush_loud")
            (expected-description "echoy_enough-xtra_field")
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (setq test-case "longer modname and description")
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
       )

     (let* ((test-case "Basic")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone-magical.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      nil)
            (expected-hyphenized  "Basic")
            (expected-subname     nil)
            (expected-description nil)
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (setq test-case "no numeric prefix")
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
       )

     (let* ((test-case "basic")
            ;; /home/doom/tmp/perlnow_test/t16/t/01-Beat-Skip-blue_stone-magical.t
            (test-file-full-path (concat t-loc test-case ".t"))
            (expected-prefix      nil)
            (expected-hyphenized  nil)
            (expected-subname     "basic")
            (expected-description nil)
            (fields (perlnow-parse-standard-t-name test-file-full-path))
            (prefix      (nth 0 fields))
            (hyphenized  (nth 1 fields))
            (subname     (nth 2 fields))
            (description (nth 3 fields))
            )
       (setq test-case "no prefix or modname, just subname")
       (assert-equal
        expected-prefix prefix (concat test-name ": " test-case ": prefix"))
       (assert-equal
        expected-hyphenized hyphenized (concat test-name ": " test-case ": hyphenized"))
       (assert-equal
        expected-subname subname (concat test-name ": " test-case ": subname"))
       (assert-equal
        expected-description description (concat test-name ": " test-case ": description"))
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
