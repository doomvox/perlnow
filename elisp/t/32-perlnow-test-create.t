#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 32-perlnow-test-create.t

;; The test story:
;;
;; Create a basic module (non-oop, exporter).
;; Insert three perl subs.

;; Do a perlnow-test-create: confirm this creates a test file
;; named using the first sub.

;; Move to near another sub, do another perlnow-test-create:
;; confirm it creates a new test file using the name of the second sub.

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

;; we'll insert this boilerplate into a newly created module buffer
(defvar test-main-perl-sub-code
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


=item stitute

=cut

sub stitute {
  my $arg = shift;

  print lc( $arg ), \"\n\";
}


")

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))
          )

     ;; make sure you know the policy in use
     (setq perlnow-test-policy-test-location   "../t")
     (setq perlnow-test-policy-dot-definition  "incspot")
     (setq perlnow-test-policy-naming-style    "fullauto")

     (setq perlnow-pm-location
           (perlnow-fixdir (concat test-loc "tcrate" perlnow-slash "lib")))
     (perlnow-ensure-directory-exists perlnow-pm-location)

     (let* (
            (funcname "perlnow-sub-at-point")
            (test-name
             (concat "Testing " funcname ))
            (package-name "Doom::Bongos")
            (expected-pm-base "Bongos.pm")
            (expected-pm-file
             (concat perlnow-pm-location "Doom" perlnow-slash expected-pm-base))
            ;; for testing, we insert two subroutines named 'mission' and 'terranean'

            (expected-t-loc
             (perlnow-fixdir
              (concat perlnow-pm-location perlnow-test-policy-test-location)))

            (expected-first-t-file
             (concat expected-t-loc "01-Doom-Bongos-mission.t"))

            (expected-second-t-file
             (concat expected-t-loc "02-Doom-Bongos-terranean.t"))

            (sub-code-str  test-main-perl-sub-code)

            first-t-buffer
            )

       ;; clear the decks
       (test-init-safe-recursive-delete expected-t-loc)
       (perlnow-ensure-directory-exists expected-t-loc)
       (test-init-move-file-out-of-way expected-pm-file)

       ;; create and open new module file
       (perlnow-module perlnow-pm-location package-name)
       (insert sub-code-str)
       (save-buffer)

       ;; move to a known point in the buffer near the first sub
       (search-backward "=item mission")
       (search-forward "sub mission")
       (move-beginning-of-line nil)

       ;; Do a create-test: confirm that creates the first test file.
       (perlnow-test-create) ;; 01-Doom-Bongos-mission.t

       (setq first-t-file (buffer-file-name))

       (assert-equal first-t-file expected-first-t-file
                     (concat test-name ": generated expected t-file in empty t dir") )

       ;;   ;; clean up test file buffer (for the hell of it?)
       ;;   (setq first-t-buffer (current-buffer))
       ;;   (other-window 1)
       ;;   (kill-buffer first-t-buffer)
       ;;   (delete-other-windows)

       ;; switch back to original module buffer
       (other-window 1)

       ;; move to a known point in the buffer near the second sub
       (search-forward "=item terranean")

       ;; Do an create-test: confirm that creates the test file.
       (perlnow-test-create) ;; 02-Doom-Bongos-terranean.t

       (setq second-t-file (buffer-file-name))

       ;;   (message "XYZ first-t-file: %s" first-t-file)
       ;;   (message "XYZ expected-first-t-file: %s" expected-first-t-file)

       (assert-equal second-t-file expected-second-t-file
                     (concat test-name ": generated expected second t-file") )
       ))
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
