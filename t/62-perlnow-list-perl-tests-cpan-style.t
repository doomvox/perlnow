#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   May 30, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/elisp/t/62-perlnow-list-perl-tests-cpan-style.t
;; Mutated from: 36-perlnow-perlnow-list-test-files.t

;;  Current targets:

;;  perlnow-tree-root
;;  perlnow-scan-tree-for-t-loc
;;  perlnow-list-perl-tests      (similar to the older perlnow-list-test-files)
;;  perlnow-list-perl-scripts
;;  perlnow-list-perl-files-in-project

;; TODO To start with, using a hardcoded t-loc to run perlnow-list-perl-tests with.
;; Next, use perlnow-scan-tree-for-t-loc to determine it.

;; The test story:

;; Run perlnow-list-test-files in different contexts: let it
;; identify a test location, and see if it returns the expected
;; set of files.

;; Here, we run on a cpan-style project (with git),
;; and start from the module file and from the test files.

;; In detail:
;;
;; create cpan-style project with milla
;;   check that the one test-file is found *from the pm file*
;; add another test ("create"), default name
;;   check that both tests are found *from the new test file*
;;   (( TODO why not repeat search from the pm file? ))
;; add a third test
;;   check that all 3 tests are found *from the new test file*
;;   check that the stash lookup finds expected inc-spot from this test

;; Contexts covered:
;; cpan-style with git (via perlnow-milla), start: pm or t:
;;   62-perlnow-list-perl-tests-cpan-style.t
;; lone module (perlnow-module), start: pm or t
;;   63-perlnow-list-perl-tests-lone-module.t

;; TODO start from pl
;;      cpan-style, but non-git

;; TODO For extra-credit: create some pathological mixed cases and see
;; how it deals with choosing a canonical "t" (it won't use more
;; than one at a time).

;; TODO
;;  o  create a script from a module, get a listing of tests from the script
;;  o  don't use a default "t" location:
;;       o  put "t" one level down rather than parallel with "lib"
;;       o  put "lib" one level down

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)
   (let* ((test-name "Testing perlnow-list-perl-tests")
          (test-loc (test-init))
          (dev-location (concat test-loc "dev"))
          )
     (message "test-loc: %s" test-loc)         ;; DEBUG
     (message "dev-location: %s" dev-location) ;; DEBUG
     (let* ((test-context-name "cpan-style") ;; TODO use this in messaging
            (package-name "Trantor::Skateboard")
            (staging-area  ;; /home/doom/tmp/perlnow_test/t62/dev/Trantor-Skateboard/
             (perlnow-staging-area dev-location package-name))
            (expected-pm (concat staging-area "lib" perlnow-slash
                                 "Trantor" perlnow-slash "Skateboard.pm") )
            (expected-Pl (concat staging-area "Build.PL"))
            (expected-t-loc (test-init-fixdir (concat staging-area "t")))
            (t-loc expected-t-loc) ;; TODO temporary
            pm-buffer
            )
       (message "staging-area: %s" staging-area)
       (test-init-safe-recursive-delete staging-area)
       (message "perlnow-dev-location: %s" perlnow-dev-location) ;; DEBUG
       (perlnow-milla perlnow-dev-location package-name)
       (assert-t (perlnow-module-code-p)
        "Testing that perlnow-milla created a module")

       (setq pm-buffer (current-buffer))
       (let* ( test-files   test-files-sorted   expected-test-files  expected-test-files-full )
         (setq expected-test-files '("01-Trantor-Skateboard.t"))
         (setq expected-test-files-full
               (mapcar (lambda (file) (concat t-loc file)) expected-test-files))
         (setq test-files
               (perlnow-list-perl-tests t-loc))
         (setq test-files-sorted
               (sort test-files 'string<))

         (assert-equal expected-test-files-full test-files-sorted
            (concat test-name ": " test-context-name ", default test files"))

         (set-buffer pm-buffer) ;; back to the pm
         (perlnow-test-create) ;; 02-Trantor-Skateboard.t
         (setq test-files
               (perlnow-list-perl-tests t-loc))
         (setq test-files-sorted
               (sort test-files 'string<))

         (setq expected-test-files
               '("01-Trantor-Skateboard.t" "02-Trantor-Skateboard.t"))
         (setq expected-test-files-full
               (mapcar (lambda (file) (concat t-loc file)) expected-test-files))

         (assert-equal expected-test-files-full test-files-sorted
            (concat test-name ": " test-context-name ", added one via perlnow-test-create"))

         (set-buffer pm-buffer) ;; back to the pm
         (perlnow-test-create) ;; 03-Trantor-Skateboard.t
         (setq test-files
               (perlnow-list-perl-tests t-loc))
         (setq test-files-sorted
               (sort test-files 'string<))

         (setq expected-test-files
               '("01-Trantor-Skateboard.t" "02-Trantor-Skateboard.t" "03-Trantor-Skateboard.t"))
         (setq expected-test-files-full
               (mapcar (lambda (file) (concat t-loc file)) expected-test-files))
         (assert-equal expected-test-files-full test-files-sorted
            (concat test-name ": " test-context-name
                ", added one more via perlnow-test-create"))

         (if perlnow-debug
             (message "perlnow-incspot-from-t-plist: %s" (pp perlnow-incspot-from-t-plist)))

         (let ((test-name-alt "Testing perlnow-stash-lookup")
               (expected-inc-spot (concat test-loc "dev/Trantor-Skateboard/lib/"))
               testfile inc-spot
               )
           (setq testfile (buffer-file-name))
           (if perlnow-debug
               (message "testfile for stash lookup: %s" (pp testfile)))
           (cond (testfile
                  (setq inc-spot
                        (perlnow-stash-lookup (file-name-directory testfile)))
                  (assert-equal expected-inc-spot inc-spot test-name-alt)
                  )))
         ))
     ) ;; end outer let*
   (end-tests)
   ))
