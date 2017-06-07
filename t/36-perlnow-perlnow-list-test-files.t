#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                 March 07, 2017
;; /home/doom/End/Cave/Perlnow/lib/perlnow/elisp/t/36-perlnow-perlnow-list-test-files.t

;; The test story:

;; For various contexts, run perlnow-list-test-files, let it
;; identify a test location, and see if it returns the
;; expected set of files.

;; Contexts include cpan-style projects and different variations
;; of non-cpan projects (with the "t" in slightly unusual relative
;; locations).  Run this on modules, tests and scripts. ((Not sure
;; tests are well-supported)).

;; TODO For extra-credit: create some pathological mixed cases and see
;; how it deals with choosing a canonical "t" (it won't use more
;; than one at a time).


;; The REAL story (Reviewing what this really does, as of: May 29, 2017)

;; create cpan-style project with milla
;;   check the one test-file is found *from the pm file*
;; add another test ("create"), default name
;;   check that both tests are found *from the new test file*
;;   (( TODO why not repeat search from the pm file? ))
;; add a third test
;;   check that all 3 tests are found *from the new test file*
;;   check that the stash lookup finds expected inc-spot from this test

;; And now, I repeat the above for a lone perlnow-module *with* a sub inserted.

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
   (let* ((test-name "Testing perlnow-list-test-files")
          (test-loc (test-init))
          (dev-location (concat test-loc "dev"))

          ;; TODO hard-coding cpan policy  Make sense?  ((El nopeo))
          (testloc    perlnow-test-policy-test-location-cpan  )
          (dotdef     perlnow-test-policy-dot-definition-cpan )
          (namestyle  perlnow-test-policy-naming-style-cpan   )

          )
     (let* ((test-context-name "cpan-style") ;; TODO use this in messaging
            (package-name "Trantor::Skateboard")
            (staging-area
             (perlnow-staging-area dev-location package-name))
            (expected-pm (concat staging-area "lib" perlnow-slash
                                 "Trantor" perlnow-slash "Skateboard.pm") )
            (expected-Pl (concat staging-area "Build.PL"))
            (expected-t-loc (test-init-fixdir (concat staging-area "t")))
            pm-buffer
            )
       (cond (perlnow-debug
              (message "test-loc: %s"     test-loc)
              (message "staging-area: %s" staging-area)
              ))
       (test-init-safe-recursive-delete staging-area)
       (perlnow-milla perlnow-dev-location package-name)
       (assert-t (perlnow-module-code-p)
        "Testing that perlnow-milla created a module")

       (setq pm-buffer (current-buffer))
       (let* (
              (fullpath-opt nil)  ;; Really, should be set to 't...
              (expected-test-files '("01-Trantor-Skateboard.t"))
              test-files   test-files-sorted
              )
         (setq test-files
               (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))

         (setq test-files-sorted
               (sort test-files 'string<))

         (assert-equal expected-test-files test-files-sorted
            (concat test-name ": " test-context-name ", default test files"))

         (set-buffer pm-buffer) ;; back to the pm
         (perlnow-test-create) ;; 02-Trantor-Skateboard.t
         (setq test-files
               (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
         (setq test-files-sorted
               (sort test-files 'string<))

         (setq expected-test-files
               '("01-Trantor-Skateboard.t" "02-Trantor-Skateboard.t"))

         (assert-equal expected-test-files test-files-sorted
            (concat test-name ": " test-context-name ", added one via perlnow-test-create"))

         (set-buffer pm-buffer) ;; back to the pm
         (perlnow-test-create) ;; 03-Trantor-Skateboard.t
         (setq test-files
               (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
         (setq test-files-sorted
               (sort test-files 'string<))

         (setq expected-test-files
               '("01-Trantor-Skateboard.t" "02-Trantor-Skateboard.t" "03-Trantor-Skateboard.t"))

         (assert-equal expected-test-files test-files-sorted
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
     ;; PART2: using pieces from 38-*.t
     (let* ((test-context-name "lone module")
            (package-name "Embobbler::ToolKit")
            (script-name "gomjabbar.pl")
            (script-base "long_walks_on_beach")
            (expected-pm-base "ToolKit")

            (staging-area
             (perlnow-staging-area dev-location package-name))
            (expected-pm (concat staging-area "lib" perlnow-slash
                                 "Embobbler" perlnow-slash "ToolKit.pm") )

            (expected-t-location
             (test-init-fixdir (concat perlnow-pm-location "../t")))

            pm-buffer script-buffer t-buffer argument-text


            (fullpath-opt nil) ;; TODO change to t, then add paths to expecteds
            ;;              (expected-test-files '("01-Embobbler-ToolKit.t"))
            (expected-test-files '())
            test-files   test-files-sorted
            )
       (cond (perlnow-debug
              (message "test-loc: %s"     test-loc)
              (message "staging-area: %s" staging-area)
              ))

       (perlnow-module perlnow-pm-location package-name)
       (assert-t (perlnow-module-code-p)
        "Testing that perlnow-module created a module")

       (setq pm-buffer (current-buffer))
       (setq expected-test-files '())
       (setq test-files
             (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
       (setq test-files-sorted
             (sort test-files 'string<))
       (assert-equal expected-test-files test-files-sorted
          (concat test-name ": " test-context-name ", default test"))

       (setq pm-buffer (current-buffer))
       (perlnow-insert-sub "sharp_pop")
       (perlnow-test-create) ;;
       (setq expected-test-files
             '("01-Embobbler-ToolKit-sharp_pop.t"))  ;;  "02-Embobbler-ToolKit.t"
       (setq test-files
             (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
       (setq test-files-sorted
             (sort test-files 'string<))

       (assert-equal expected-test-files test-files-sorted
          (concat test-name ": " test-context-name ",  add via perlnow-test-create"))

       (set-buffer pm-buffer) ;; back to the pm
       (perlnow-test-create)
       (setq test-files
             (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
       (setq test-files-sorted
             (sort test-files 'string<))

       (setq expected-test-files
             '("01-Embobbler-ToolKit-sharp_pop.t" "02-Embobbler-ToolKit-sharp_pop.t"))
       (assert-equal expected-test-files test-files-sorted
        (concat test-name ": " test-context-name ", another via perlnow-test-create"))

       (if perlnow-debug
           (message "perlnow-incspot-from-t-plist: %s" (pp perlnow-incspot-from-t-plist)))

       (let ((test-name-alt "Testing perlnow-stash-lookup")
             (expected-inc-spot (concat test-loc "lib/"))
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
       ) ;; end let* for PART2
     ) ;; end outer let*
   (end-tests)
   ))
