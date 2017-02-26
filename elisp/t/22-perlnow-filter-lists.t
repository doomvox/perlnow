#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 22-perlnow-filter-lists.t

;; The test story:
;; Check some primitives that filter lists and lists of lists in
;; different ways.

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(defun test-main-agonostises ()
  "A standard wrapper around tests, with name silly enough to dodge collisions.
Simplifies running edebug."
  (interactive) ;;?

  (if (file-exists-p "test-init-elisp.el")
    (load-file "test-init-elisp.el"))

 (setenv "USE_TAP" "t")

;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
;; TODO
;;  install the latest, maybe via emacs package management: should have my fix.
  (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")

  (test-simple-start) ;; Zero counters and start the stop watch.

  (setq perlnow-force t) ;; ask me no questions

;;  (load-file "/home/doom/End/Cave/Perlnow/lib/perlnow/elisp/perlnow.el")

  (let* (
         (funcname "perlnow-filter-list")
         (test-name
          (concat "Testing " funcname ))

         (input-list (list "alpha" "beta" "gamma" "delta" "epsilon" "eta"))
         (rm-pat "^ep")

         (result-list (perlnow-filter-list input-list rm-pat))
         (expected-list (list "alpha" "beta" "gamma" "delta" "eta"))
       )

;;     (message "input: %s" (pp input-list))
;;     (message "result: %s" (pp result-list))
;;     (message "expected: %s" (pp expected-list))

    (assert-t
     (equal result-list expected-list)
     (concat test-name ": removing one element") )

    )

  (let* (
         (funcname "perlnow-filter-list")
         (test-name
          (concat "Testing " funcname ))
         (input-list (list "alpha" "beta" "gamma" "delta" "epsilon" "eta"))
         (rm-pat "p")
         (expected-list (list "beta" "gamma" "delta" "eta"))
         (result-list (perlnow-filter-list input-list rm-pat))
       )

;;     (message "input: %s" (pp input-list))
;;     (message "result: %s" (pp result-list))
;;     (message "expected: %s" (pp expected-list))

    (assert-t
     (equal result-list expected-list)
     (concat test-name ": removing two elements") )

    )

  (let* (
         (funcname "perlnow-grep-list")
         (test-name
          (concat "Testing " funcname ))

         (input-list (list "alpha" "beta" "gamma" "delta" "epsilon" "eta"))
         (keep-pat "^ep")

         (result-list (perlnow-grep-list input-list keep-pat))
         (expected-list (list "epsilon"))
       )

;;     (message "input: %s" (pp input-list))
;;     (message "result: %s" (pp result-list))
;;     (message "expected: %s" (pp expected-list))

    (assert-t
     (equal result-list expected-list)
     (concat test-name ": keeping one element") )

    )

  (let* (
         (funcname "perlnow-grep-list")
         (test-name
          (concat "Testing " funcname ))
         (input-list (list "alpha" "beta" "gamma" "delta" "epsilon" "eta"))
         (keep-pat "p")
         (expected-list (list "alpha" "epsilon"))
         (result-list (perlnow-grep-list input-list keep-pat))
       )

;;     (message "input: %s" (pp input-list))
;;     (message "result: %s" (pp result-list))
;;     (message "expected: %s" (pp expected-list))

    (assert-t
     (equal result-list expected-list)
     (concat test-name ": keeping two elements") )
    )


;;(defun perlnow-minimum-nonempty-list (list-of-lists)

  (let* (
         (funcname "perlnow-minimum-nonempty-list")
         (test-name
          (concat "Testing " funcname ))
         (list-a (list "alpha" "beta" "gamma" "delta" "epsilon" "eta"))
         (list-b (list "spa" "fon" "squa" "tront"))
         (list-c (list "burroughs" "ginsberg" "kerouac"))
         (input (list list-a list-b list-c))

         (expected list-c)
         (result (perlnow-minimum-nonempty-list input))
       )
;;     (message "input: %s" (pp input))
;;     (message "result: %s" (pp result))
;;     (message "expected: %s" (pp expected))

    (assert-t
     (equal result expected)
     (concat test-name ": find shortest of three (nonempties)"))
    )

  (let* (
         (funcname "perlnow-minimum-nonempty-list")
         (test-name
          (concat "Testing " funcname ))
         (list-a ())
         (list-b ())
         (list-c (list "yestersnow"))
         (input (list list-a list-c list-b))

         (expected list-c)
         (result (perlnow-minimum-nonempty-list input))
       )
;;     (message "input: %s" (pp input))
;;     (message "result: %s" (pp result))
;;     (message "expected: %s" (pp expected))

    (assert-t
     (equal result expected)
     (concat test-name ": find the only nonempty of three"))
    )

  (let* (
         (funcname "perlnow-minimum-nonempty-list")
         (test-name
          (concat "Testing " funcname ))
         (list-a (list "a" "b" "c"))
         (list-b (list "1" "2" "3"))
         (list-c (list "dolls" "hell" "smith" ))
         (input (list list-a list-c list-b))

         (expected list-a)
         (result (perlnow-minimum-nonempty-list input))
       )
    (message "input: %s" (pp input))
    ;;     (message "result: %s" (pp result))
    ;;     (message "expected: %s" (pp expected))qcv c09vb

    (assert-t
     (equal result expected)
     (concat test-name ": break a tie between lists of equal length"))
    )
  )

(test-main-agonostises)

(end-tests)

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
