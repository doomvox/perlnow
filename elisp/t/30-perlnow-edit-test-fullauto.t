#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; perlnow-edit-test-fullauto

;; The test story:
;;
;; Create a basic module (non-oop, exporter).
;; Insert a couple of sub routines.
;;
;; Do an edit-test: confirm that creates the first test file.
;; Delete that test file.  Manually create two oddly named misc test files.
;; Do an edit-test: confirm that creates the first test file
;; with correct naming convention, numbered in sequence after the others.
;;

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
")

(funcall
 (lambda ()
   (if (file-exists-p "test-init-elisp.el")
       (load-file "test-init-elisp.el"))

   ;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
   (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))
          )

     ;; make sure you know the policy in use
     (setq perlnow-test-policy-test-location   "../t")
     (setq perlnow-test-policy-dot-definition  "incspot")
     (setq perlnow-test-policy-naming-style    "fullauto")

     (setq perlnow-pm-location
           (perlnow-fixdir (concat test-loc "et_fa" perlnow-slash "lib")))
     (perlnow-ensure-directory-exists perlnow-pm-location)

     (let* (
            first-t-file first-t-buffer

                         (funcname "perlnow-edit-test-file")
                         (test-name
                          (concat "Testing " funcname ))
                         (package-name "Bongos::Doom")
                         (expected-pm-base "Doom.pm")
                         (expected-pm-file
                          (concat perlnow-pm-location "Bongos" perlnow-slash expected-pm-base))
                         ;; for testing, we insert two subroutines named 'mission' and 'terranean'

                         (expected-t-loc
                          (perlnow-fixdir
                           (concat perlnow-pm-location perlnow-test-policy-test-location)))

                         (expected-first-t-file
                          (concat expected-t-loc "01-Bongos-Doom-mission.t"))

                         (sub-code-str  test-main-perl-sub-code)
                         )

       ;;     (message "*** expected-pm-file: %s " expected-pm-file)
       ;;     (message "*** expected-t-loc: %s " expected-t-loc)

       ;; clear the decks
       (test-init-safe-recursive-delete expected-t-loc)
       (perlnow-ensure-directory-exists expected-t-loc)

       ;; create and open new module file
       (perlnow-module perlnow-pm-location package-name)
       (insert sub-code-str)
       (save-buffer)

       ;; move to a known point in the buffer
       (search-backward "=item mission")
       (search-forward "sub mission")
       (move-beginning-of-line nil)

       ;; Do an edit-test: confirm that creates the first test file.
       (perlnow-edit-test-file) ;; 01-Bongo-Doom-mission.t

       (setq first-t-file (buffer-file-name))

       ;;   (message "XYZ first-t-file: %s" first-t-file)
       ;;   (message "XYZ expected-first-t-file: %s" expected-first-t-file)

       (assert-t
        (string= first-t-file expected-first-t-file)
        (concat test-name ": generated expected t-file in empty t dir") )

       ;; clean up test file buffer
       (setq first-t-buffer (current-buffer))
       (other-window 1)
       (kill-buffer first-t-buffer)
       (delete-other-windows)

       ;; Delete that test file.
       (test-init-move-file-out-of-way expected-first-t-file)

       ;; manually create some oddly named misc test files.
       (let* (
              t-file
              (t-loc expected-t-loc)
              (t-list (list "01-yo.t" "12-up.t" "03-rat.t" "27-out.t"))
              (expected-t-file
               (concat expected-t-loc "28-Bongos-Doom-mission.t"))
              )
         (dolist (file t-list)
           (let ( (cmd
                   (concat "touch " t-loc file)))
             (call-process-shell-command cmd)
             ))

         (perlnow-edit-test-file) ;; 28-Bongo-Doom-mission.t

         (setq t-file (buffer-file-name))

         ;;   (message "123 first-t-file: %s" first-t-file)
         ;;   (message "123 expected-first-t-file: %s" expected-first-t-file)

         (assert-equal t-file expected-t-file
                       (concat test-name ": generated t-file numbered in sequence after others") )
         )))
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
