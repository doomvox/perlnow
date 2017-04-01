#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;;; 05-perlnow-from-pm-to-t-check-and-back.t

;; The test story:
;;   module starter
;;   edit test
;;   run check
;;   back to code

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   ;; project-specific include file (with standard name)
    (if (file-exists-p "test-init.el")
        (load-file "test-init.el"))
    (perlnow-tron)
   (let* (
          (test-loc (test-init))
          (funcname "perlnow-module-starter")
          (test-name
           (concat "Testing that " funcname " creates cpan-style module"))
          (package-name "Lost::In::Test")
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))
          (expected-pm-base "Test.pm")
          (expected-pm-file  ;; ... lib/Lost/In/Test.pm
           (concat
            (file-name-as-directory (concat staging-area "lib"))
            "Lost" perlnow-slash "In" perlnow-slash expected-pm-base))
          (expected-pl (concat staging-area "Build.PL"))
          (expected-t
           (concat
            (file-name-as-directory (concat staging-area "t")) "01-Lost-In-Test.t"))
          pm-buffer
          )
     (test-init-safe-recursive-delete staging-area)

     (perlnow-module-starter perlnow-dev-location package-name)

     (let* (t-exists-p pm-exists-p PL-exists-p t-found-p
                       check-ok-p check-harder-ok-p back-worked-p)

       ;; First, some redundant tests covered in 03-*.t already.
       (setq pm-exists-p
             (assert-t
              (file-exists-p expected-pm-file)
              (concat test-name " " expected-pm-file ) ))  ;; ok 1

       (setq PL-exists-p
             (assert-t
              (file-exists-p expected-pl)
              (concat "Testing that " funcname " generated expected Build.PL file" ))) ;; ok 2

       (setq t-exists-p
             (assert-t
              (file-exists-p expected-t)
              (concat "Testing that " funcname " generated expected *.t file" ))) ;; ok 3

       (cond ((and pm-exists-p t-exists-p)
              ;; /home/doom/tmp/perlnow_test/lib/Lost-In-Test/
              ;;    lib/Lost/In/Test.pm
              (find-file expected-pm-file)
              (setq pm-buffer (current-buffer))

;;              (perlnow-edit-test-file)     ;; TODO this brings up the select menu buffer!  WTF?
;;              (perlnow-edit-test-file nil)     ;; TODO how about this?  Just saw same behavior in edebug-- Fri  March 31, 2017  14:49
              ;; (perlnow-edit-test-file 1)  ;; a harder-setting of less-than-four

              ;; Faking a simple "edit-test" call, without arguments
              (perlnow-open-test-file
               (perlnow-get-test-file-name))

;;               ;; DEBUGING coping out on interactive issue, forcing the t-file open
;;               (cond ((perlnow-test-select-menu-p)
;;                      (switch-to-buffer pm-buffer)
;;                      (perlnow-edit-test-file expected-t)  ;; this brings up select test menu too!
;;                       ))

              (if perlnow-debug
                  (message "TRALALA: the t I hope: %s" (pp-to-string (buffer-file-name))))
              (setq t-found-p
                    (assert-t
                     (string= (buffer-file-name) expected-t)
                     (concat "Testing that perlnow-edit-test-file found *.t file"))) ;; ok 4
              ))

       (cond (pm-exists-p
              (let* ((syntax-ok-pat (concat expected-pm-base " syntax OK"))
                     (syntax-pod-ok-pat (concat expected-pm-base " pod syntax OK"))
                     (critic-ok-pat (concat expected-pm-base " source OK"))
                     )
                (find-file expected-pm-file)
                (perlnow-run-check nil)
                (sleep-for 2) ;; wait for compile (only thing in emacs that's async)
                (set-buffer "*compilation*")
                (let* ( ( compilation-results (buffer-string) )
                        )
                  ;; (message "%s" compilation-results) ;; DEBUG
                  (setq check-ok-p
                        (assert-t
                         (string-match syntax-ok-pat compilation-results)
                         (concat "Testing that perlnow-run-check worked."))) ;; ok 5
                  )

                (find-file expected-pm-file)
                (perlnow-run-check 4) ;; run it "harder"
                (sleep-for 2) ;; wait for compile (only thing in emacs that's async)
                (set-buffer "*compilation*")
                (let* ( ( compilation-results (buffer-string) )
                        ( syntax-ok-1
                          (string-match syntax-ok-pat     compilation-results))
                        ( syntax-ok-2
                          (string-match syntax-pod-ok-pat compilation-results))
                        ( critic-ok
                          (string-match critic-ok-pat     compilation-results))
                        )
                  (cond (perlnow-debug
                         (message "%s" compilation-results)
                         (message "so1: %s  so2: %s  co: %s" syntax-ok-1 syntax-ok-2 critic-ok)
                         ;; so1: 235  so2: 330  co: 353
                         ;; so1: 239  so2: 334  co: 357   -- Wed  March 08, 2017  23:27
                         ))
                  (setq check-harder-ok-p
                        (assert-t
                         (and syntax-ok-1 syntax-ok-2 critic-ok)
                         (concat "Testing that perlnow-run-check harder worked."))) ;; ok 6
                  )
                )))

       (cond (check-ok-p
              ;; /home/doom/tmp/perlnow_test/lib/Lost-In-Test/
              ;;    lib/Lost/In/Test.pm
              (find-file expected-pm-file)
;;              (perlnow-edit-test-file)   ;; TODO can be select menu buffer, but not always?
;;              (perlnow-edit-test-file nil)     ;; TODO experimental.  Okay?
;;              (perlnow-edit-test-file)
;;              (perlnow-edit-test-file 1)
              ;; Faking a simple "edit-test" call, without arguments
              (perlnow-open-test-file
               (perlnow-get-test-file-name))
              (perlnow-back-to-code)     ;; ... but if so this still works.
              ;; Now, we should be back in the original pm file.
              (setq back-worked-p
                    (assert-equal
                     (buffer-file-name) expected-pm-file
                     (concat "Testing that perlnow-back-to-code found pm file"))) ;; ok 7
              ))
       ))
   (end-tests)
   ))





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
