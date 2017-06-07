#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; /home/doom/End/Cave/Perlnow/lib/perlnow/elisp/t/56-perlnow-useok-scraping.t

;; The test story:

;; To begin with, just run a simple routine that generates a regexp.
;; (a) Look at the regexp.
;; (b) Run it against a stack of test cases

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
          (test-name "Testing useok scraping regexp")
          (capture-pat (perlnow-package-capture-from-useok-pat))
          )

     ;; (a) doing a visual inspection
     ;;  (message "capture-pat: \n %s" (pp-to-string capture-pat))
     ;; capture-pat:
     ;;  "^[ 	]*?use_ok(?[ 	]*?\\(\\(?:['\"]\\|q[^x]\\).*?\\)\\(?:,\\|[ 	]*?)?[ 	]*?;\\)"

     ;; (b) try it against test cases
     (let* (
            (case1 "use_ok( 'WildSide::Glam' ,  qw(sashay) );")
            (expect1 "'WildSide::Glam' ")

            (cases
             (list
              "use_ok( 'WildSide::Glam' ,  qw(sashay) );"
              " use_ok( 'WildSide::Glam' ,  qw(sashay) );"
              " use_ok 'WildSide::Glam' ,  qw(sashay) ;"
              " use_ok q{WildSide::Glam} ,  qw(sashay) ;"
              " use_ok qq{WildSide::Glam} ,  qw(sashay) ;"
              " use_ok qq|WildSide::Glam| ,  qw(sashay) ;"
              " use_ok qq|WildSide::Glam|  ;"
              " use_ok( 'WildSide::Glam'  );"
              " use_ok( 'WildSide::Glam'  ) ;"
              " use_ok( q{WildSide::Glam} ) ;"
              " use_ok( q{WildSide::Glam}) ;"
              "use_ok(q{WildSide::Glam}) ;"
              "use_ok(qq{WildSide::Glam}) ;"
              "use_ok(qq(WildSide::Glam)) ;"
              "use_ok(qq( WildSide::Glam )) ;"
              "use_ok(qq(WildSide::Glam)) ;"
              ))
            (expect-package "WildSide::Glam")
            result
            )

       (message "case1: %s" case1)
       (string-match capture-pat case1)
       (setq result
             (match-string 1 case1))
       (message "result: %s" result)
       (assert-equal
         expect1 result
         (concat test-name ": full test of case: " case1))

        (dolist (case cases)
          ;; (message "case: %s" case)
          (string-match capture-pat case)
          (setq result
                (match-string 1 case))
          ;; crude technique to strip it down to the package name
          (setq result
                (perlnow-strip-perl-quotage result))
          ;; (message "TRM result: %s" result)
          (assert-equal
           expect-package result
           (concat test-name ": rough test of case: " case))
          )
       )

     (let* ((test-name "Testing perlnow-get-package-name-from-useok")
            (expected-package "Gorgonzolla::Getaway")
            (t-name "01-Gorgonzolla-Getaway.t")
            (slash test-init-slash) ;; on unix, "/"
            (data-loc (concat default-directory "dat/"))
            (source-loc (concat data-loc "code" slash "s56" slash))
            (source-t   (concat source-loc "t"  slash t-name))
            (t-loc   (test-init-fixdir (concat perlnow-pm-location slash ".." slash "t")))
            (t-full  (concat t-loc slash t-name))
            package-name
            )
       ;; Initialize the t56 tree with t file in dat/code
       (test-init-mkpath t-loc)
       (copy-file source-t t-loc t)
       (find-file t-full)
       (setq package-name (perlnow-get-package-name-from-useok))
       (assert-equal
        expected-package package-name
        (concat test-name ": found package name: " expected-package ))
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
