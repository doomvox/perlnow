#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   May 30, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/elisp/t/63-perlnow-list-perl-tests-lone-module.t

;; See discussion at top of: 62-perlnow-list-perl-tests-cpan-style.t
;; (this used to be called PART2)

;; The test story:
;;   o  create a module (non-cpan, non-git)
;;   o  list tests (starting from module buffer): verify there are none
;;   o  create a test
;;   o  list tests (again from pm buffer): verify there's one, and it's named as expected.
;;   o  create a test 
;;   o  list tests (again, from pm buffer): verify there's two, and both are named as expected.

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)
   (let* ((test-name "Testing perlnow-list-perl-tests")
          (test-loc (test-init))
          ;; (dev-location (concat test-loc "dev"))
          (slash test-init-slash)
          )
     (let* ((test-context-name "lone module")
            (package-name "Embobbler::ToolKit")
            ;; (script-name "gomjabbar.pl")
            ;; (script-base "long_walks_on_beach")
            (lib-location        (test-init-fixdir (concat test-loc "lib")))
               ;; note: that's the same as the perlnow-lib-location...
            (expected-t-location (test-init-fixdir (concat test-loc "t")))
            (expected-pm (concat lib-location
                               "Embobbler" slash "ToolKit.pm") )
            (t-loc expected-t-location) ;; convenience

            pm-buffer    
            test-files   test-files-sorted  expected-test-files  expected-test-files-full
            )
       (assert-equal lib-location perlnow-pm-location "Testing that I am not a retard.")

       (perlnow-module perlnow-pm-location package-name)
       (assert-t (perlnow-module-code-p)
        "Testing that perlnow-module created a module")
       (setq expected-test-files '())

       (setq pm-buffer (current-buffer))
       (setq test-files (perlnow-list-perl-tests)) ;; sans arg, checks default-directory
       (setq test-files-sorted
             (sort test-files 'string<))

       (assert-equal expected-test-files test-files-sorted
          (concat test-name ": " test-context-name ", none created yet"))

       (setq pm-buffer (current-buffer))
       (perlnow-insert-sub "sharp_pop")
       (perlnow-test-create) ;;

       (setq expected-test-files
             '("01-Embobbler-ToolKit-sharp_pop.t"))
       (setq expected-test-files-full
             (mapcar (lambda (file) (concat t-loc file)) expected-test-files))

       (set-buffer pm-buffer)
       (setq test-files (perlnow-list-perl-tests)) ;; sans arg, checks default-directory
       (setq test-files-sorted
             (sort test-files 'string<))

       (assert-equal expected-test-files-full test-files-sorted
          (concat test-name ": " test-context-name ",  add via perlnow-test-create"))

       (set-buffer pm-buffer) ;; back to the pm
       (perlnow-test-create)

       (set-buffer pm-buffer) ;; and back to the pm again
       (setq test-files (perlnow-list-perl-tests)) ;; sans arg, checks default-directory
       (setq test-files-sorted
             (sort test-files 'string<))

       (setq expected-test-files
             '("01-Embobbler-ToolKit-sharp_pop.t" "02-Embobbler-ToolKit-sharp_pop.t"))
       (setq expected-test-files-full
             (mapcar (lambda (file) (concat t-loc file)) expected-test-files))
       (assert-equal expected-test-files-full test-files-sorted
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
       ) ;; end let* 
     ) ;; end outer let*
   (end-tests)
   ))
