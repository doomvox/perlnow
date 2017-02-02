#!/usr/local/bin/emacs --script
;; /usr/bin/emacs

;;; 03-perlnow-module-starter.t

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(if (file-exists-p "test-init-elisp.el")
    (load-file "test-init-elisp.el"))

(setenv "USE_TAP" "t")

;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
;; TODO install the latest, maybe via emacs package management: should have my fix.
(load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")

(test-simple-start) ;; Zero counters and start the stop watch.

(setq perlnow-force t) ;; ask me no questions

(let* (
       (funcname "perlnow-module-starter")
       (test-name
        (concat "Testing that " funcname " creates cpan-style module"))
       (package-name "Three::Warp::Nine")
       (staging-area
        (perlnow-staging-area perlnow-pm-location package-name))
       (expected-file (concat staging-area "lib" "/Three/Warp/Nine.pm"))
       (expected-Pl (concat staging-area "Build.PL"))
       )
  (perlnow-mkpath perlnow-pm-location)
  (perlnow-mkpath staging-area)

  (perlnow-module-starter perlnow-pm-location package-name)

  (assert-t
   (file-exists-p expected-file)
   (concat test-name " " expected-file ) )

  (assert-t
   (file-exists-p expected-Pl)
   (concat "Testing that " funcname " generated expected Build.PL file" ) )

  )

;; (message "end is near")
(end-tests)
;; (message "after the end")

;; TODO BUG
;; test-simple buglet: end-tests is echoing name of *scratch* buffer to stdout, for some reason

;; end is near
;; *scratch*
;; ok 1 - Testing that perlnow-module-starter creates cpan-style module /home/doom/tmp/perlnow_test/lib/Three-Warp-Nine/lib/Three/Warp/Nine.pm
;; 1..1

;; after the end


;;========
;; LICENSE

;; This program is free software; you can redistribute it and/or modify
;; it under the same terms as the version of GNU Emacs you intend to use it with.

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
