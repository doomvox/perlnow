#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 42-perlnow-weird-lib-inside-t-loc.t

;; Test story:

;; o  Create a cpan project: Dank::Rank
;; o  Manually create an unusual "lib" inside of the "t".
;; o  Create a module in there:  t/lib/Testoidal/Ute.pm
;;   o   C-c / R has been known to crash like so:

;;   warning .../Dank-Rank/t/t/ is not a directory
;;   setq: Opening directory: no such file or directory, .../Dank-Rank/t/t/

;; Contrive a non-interactive test to excersize that.

;; Preferred behavior: give up on guessing if impossible (or just too weird),
;;                     but keep going with an empty string.
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
   (let* (
          (test-loc (test-init))
          (test-name "")

          (package-name "Dank::Rank")
          (pm-base "Rank")
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))

          (cpan-lib (concat staging-area test-init-slash "lib" test-init-slash))
          (cpan-t   (concat staging-area test-init-slash "t" test-init-slash))

          (weird-lib (concat cpan-t test-init-slash "lib" test-init-slash))
          (nouveau-package-name "Testoidal::Ute")

;;          (exp-rs-from-pm "") ;; TODO

          ;; Because this is a 'weird' case with detailed UI still up for grabs
          ;; there's no one expected, we allow two:
          (allowed-rs-from-pm (list ""  "perl /home/doom/tmp/perlnow_test/t42/dev/Dank-Rank/t/01-Dank-Rank.t"))

          ;; (perl "/usr/bin/perl") ;; same has hash-bang in script template
          (perl "perl") ;; but much better to let the PATH sort it out

          rs-from-pm

          t-result
         )
    (message "SaaaLEEEze me...") ;; DEBUG

    ;; o  Create a cpan project: Dank::Rank
    (perlnow-milla perlnow-dev-location package-name)

    ;; o  Manually create an unusual "lib" inside of the "t".
    (test-init-mkpath weird-lib)

    ;; o  Create a module in there:  t/lib/Testoidal/Ute.pm
    (perlnow-module weird-lib nouveau-package-name)

    ;; The problem was with the attempt at coming up with a good
    ;; guess for (perlnow-set-run-string), that's hard to test
    ;; non-interactively, so we'll go up the chain to "guess":

    (setq rs-from-pm
          (perlnow-guess-run-string)) ;; acts on current perl buffer

    (message "Survived!") ;; DEBUG

    (setq t-result
          (catch 'OUT 
            (dolist (expected allowed-rs-from-pm)
              (if (equal expected rs-from-pm)
                  (throw 'OUT t))
              )))


    (assert-t t-result 
              (concat "Testing perlnow-guess-run-string for pm "))

;;     ;; TODO
;;     ;; Preferred behavior: give up on guessing if impossible (or just too weird),
;;     ;;                     but keep going with an empty string.

;;       (assert-equal exp-rs-from-pm rs-from-pm
;;                   (concat "Testing perlnow-guess-run-string for pm "))
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

;;; 42-perlnow-weird-lib-inside-t-loc.t ends here
