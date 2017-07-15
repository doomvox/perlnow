#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   May 30, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/elisp/t/63-perlnow-list-perl-tests-lone-module.t
;; Mutated from: 36-perlnow-perlnow-list-test-files.t

;; See discussion at top of: 62-perlnow-list-perl-tests-cpan-style.t

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)
   (let* ((test-name "Testing perlnow-list-perl-tests")
          (test-loc (test-init))
          (dev-location (concat test-loc "dev"))
          )
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

            (expected-t-loc (test-init-fixdir (concat perlnow-pm-location "../t")))
            (t-loc expected-t-loc) ;; TODO temporary

            pm-buffer script-buffer t-buffer argument-text
            test-files   test-files-sorted  expected-test-files  expected-test-files-full
            )
       (perlnow-module perlnow-pm-location package-name)
       (assert-t (perlnow-module-code-p)
        "Testing that perlnow-module created a module")

       (setq pm-buffer (current-buffer))
       (setq expected-test-files '())

       ;; (message "Reading the tea leaves and the t-loc: %s" (pp-to-string t-loc)) ;; DEBUG
       ;; (setq test-files (perlnow-list-perl-tests t-loc))
       (setq test-files (perlnow-list-perl-tests staging-area))
       (setq test-files-sorted
             (sort test-files 'string<))

       ;; (message "^v^A^v^")
       (assert-equal expected-test-files test-files-sorted
          (concat test-name ": " test-context-name ", none created yet"))

       (setq pm-buffer (current-buffer))
       (perlnow-insert-sub "sharp_pop")
       (perlnow-test-create) ;;

       (setq expected-test-files
             '("01-Embobbler-ToolKit-sharp_pop.t"))
       (setq expected-test-files-full
             (mapcar (lambda (file) (concat t-loc file)) expected-test-files))
       ;; (message "expected-test-files-full: %s" (pp-to-string expected-test-files-full));; DEBUG

       ;; (setq test-files (perlnow-list-perl-tests t-loc))
       (setq test-files (perlnow-list-perl-tests staging-area))
       (setq test-files-sorted
             (sort test-files 'string<))
       ;; (message "^v^B^v^")
       (assert-equal expected-test-files-full test-files-sorted
          (concat test-name ": " test-context-name ",  add via perlnow-test-create"))

       (set-buffer pm-buffer) ;; back to the pm
       (perlnow-test-create)
       ;; (setq test-files (perlnow-list-perl-tests t-loc))
       (setq test-files (perlnow-list-perl-tests staging-area))
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
       ) ;; end let* for PART2
     ) ;; end outer let*
   (end-tests)
   ))
