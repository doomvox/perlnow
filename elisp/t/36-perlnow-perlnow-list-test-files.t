#!/usr/local/bin/emacs --script
;;    /usr/bin/emacs
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

;; For extra-credit: create some pathological mixed cases and see
;; how it deals with choosing a canonical "t" (it won't use more
;; than one at a time).

;; I'm writing this test because it's a mid-level feature in the
;; space where I'm going to be working next, and I'm not entirely
;; confident that it's working right for all cases-- I have a
;; note about how it might be cpan-specific, for example.

(funcall
 (lambda ()
   (if (file-exists-p "test-init-elisp.el")
       (load-file "test-init-elisp.el"))

   ;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
   (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))
          (test-name "Testing perlnow-list-test-files")
          (package-name "Trantor::Skateboard")
          (staging-area
           (perlnow-staging-area perlnow-dev-location package-name))
          (expected-project-loc
           (perlnow-fixdir
            (concat staging-area "Trantor-Skateboard")))
          (expected-pm (concat expected-project-loc "lib" perlnow-slash
                                 "Trantor" perlnow-slash "Skateboard.pm") )
          (expected-Pl (concat staging-area "Build.PL"))
          (expected-t-loc (perlnow-fixdir (concat expected-project-loc "t")))
          pm-buffer
          )

;;      (require 'template)
;;      (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

     (test-init-safe-recursive-delete expected-project-loc)
     (perlnow-milla expected-project-loc package-name)

     (assert-t
      (perlnow-module-code-p)
      "Testing that perlnow-milla created a module")

     (setq pm-buffer (current-buffer))

     (let* (
            ;; TODO hard-coding cpan policy (I think)  Make sense?
            (testloc    perlnow-test-policy-test-location-cpan  )
            (dotdef     perlnow-test-policy-dot-definition-cpan )
            (namestyle  perlnow-test-policy-naming-style-cpan   )

            (fullpath-opt nil)
            (expected-test-files '("01-Trantor-Skateboard.t" "basic.t"))
            test-files   test-files-sorted
            )

            (setq test-files
                  (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))

            (setq test-files-sorted
                  (sort test-files 'string<))

            ;; (message (pp test-files))
            ;; ("01-Trantor-Skateboard.t" "basic.t")
            ;; (message (pp test-files-sorted))
            ;; ("01-Trantor-Skateboard.t" "basic.t")

            (assert-equal
             expected-test-files
             test-files-sorted
             (concat test-name ": default tests from perlnow-milla"))

            (set-buffer pm-buffer) ;; back to the pm

            (perlnow-test-create) ;; 02-Trantor-Skateboard.t

            ;; For some reason, this is necessary:
            ;; (set-buffer pm-buffer) ;; back to the pm

            (perlnow-tron)
            (setq test-files
                  (perlnow-list-test-files testloc dotdef namestyle fullpath-opt))
            (setq test-files-sorted
                  (sort test-files 'string<))

            (setq expected-test-files '("01-Trantor-Skateboard.t" "02-Trantor-Skateboard.t" "basic.t"))

            (assert-equal
             expected-test-files
             test-files-sorted
             (concat test-name ": additional via perlnow-test-create"))


       )
     ) ;; end let*
   (end-tests)
   ))
