#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 20-perlnow-sub-at-point.t

;; The test story:
;;
;; Create a basic module (non-oop, exporter).
;; Insert a couple of sub routines.
;;
;; Check the return from perlnow-sub-at-point as you move to
;; different places in and near the subs.
;;
;; (The old version had a bug, where in the region between
;; subs it reports the name of the one above, when the right
;; thing is to report the name of the one below.)

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

          (funcname "perlnow-sub-at-point")
          (test-name (concat "Testing " funcname ))
          (package-name "Modular::Stuff")
          (expected-pm-base "Stuff.pm")
          (expected-pm-file
           (concat perlnow-pm-location "Modular" perlnow-slash expected-pm-base))
          ;; for testing, we insert two subroutines named 'mission' and 'terranean'
          (sub-code-str
           "
=item mission

=cut

# Talking about my mission
sub mission {
  my $arg = shift;

  print uc( $arg ), \"\n\";
}



=item terranean

=cut

sub terranean {
  my $arg = shift;

  print lc( $arg ), \"\n\";
}
")

          ;;       (perl "/usr/bin/perl")
          returned-sub
          )

     ;; delete existing *.pm file (( TODO better to rename out of the way? ))
     ;;   (if (file-exists-p expected-pm-file)
     ;;       (delete-file expected-pm-file))
     (test-init-move-file-out-of-way expected-pm-file)

     ;; create and open new module file
     (perlnow-module perlnow-pm-location package-name)
     (insert sub-code-str)
     (save-buffer)

     ;; begin out-of-sequence with an easy one
     (search-backward "=item mission")
     (search-forward "sub mission")
     (move-beginning-of-line nil)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub "mission")
      (concat test-name ": beginning of first sub") )

     ;; now, start at beginning of two subs pod, and step forward
     (search-backward "=item mission")
     (setq returned-sub (perlnow-sub-at-point))
     ;; currently, nil, want it to be "mission"
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": at start of first sub pod") )

     (forward-line 1)
     (setq returned-sub (perlnow-sub-at-point))
     ;; currently, nil, want it to be "mission"
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": inside the first subs pod") )

     (search-forward "=cut")
     (setq returned-sub (perlnow-sub-at-point))
     ;; currently, nil, want it to be "mission"
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": last line of first sub's pod") )

     (search-forward "# Talking")
     (setq returned-sub (perlnow-sub-at-point))
     ;; currently, nil, want it to be "mission"
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": comment right above first sub") )

     (search-forward "sub mission")
     (move-beginning-of-line nil)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": beginning of first sub") )

     (forward-word 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": between sub keyword and subname, first sub") )

     (move-end-of-line nil)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": eol, first line of first sub") )

     (forward-line 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": inside of the first sub") )

     (search-forward "}")
     (backward-char 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "mission")
      (concat test-name ": at closing brace of first sub") )

     (search-forward "=item terranean")
     (previous-line 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": between first sub and pod of second") )

     (search-forward "=item terranean")
     (move-beginning-of-line nil)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": start of pod for second sub") )

     (search-forward "=item terranean")
     (forward-line 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": inside pod for second sub") )

     (search-forward "sub ")
     (move-beginning-of-line nil)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": beginning of second sub") )

     (forward-word 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": first line of second sub") )

     (search-forward "{")
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": opening brace of second sub") )

     (backward-char 1)
     (forward-sexp 1)
     (backward-char 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-t
      (string= returned-sub  "terranean")
      (concat test-name ": closing brace of second sub") )

     (forward-line 1)
     (setq returned-sub (perlnow-sub-at-point))
     (assert-nil
      (string= returned-sub  "terranean")
      (concat test-name ": after end of second sub") )

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
