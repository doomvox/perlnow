#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   June 18, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/elisp/t/68-perlnow-test-harder-select-menu.t
;; Mutated from: 65-perlnow-list-perl-tests-no-history.t

;; Starting with 65-*.t to use the code that reads in the meta file
;; looping over All Cases is okay too, even if there's just one case

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)

   ;; debugging convenience hack
   (let ((my-dev "/home/doom/End/Cave/Perlnow/lib/perlnow/t/"))
     (if (file-accessible-directory-p my-dev)
         (cd my-dev)))

   ;; "no history" means we're looking at files not created by perlnow:
   ;; perlnow doesn't know their history.
   (let* ((test-name "Testing perlnow-list-perl-tests with no history")
          (slash test-init-slash)
          (test-loc (test-init))  ;; /home/doom/tmp/perlnow_test/t65/
          (dev-location (concat test-loc "dev"))
          (test-set        (concat test-loc "test_set" slash))
          (test-case-file  (concat test-set "meta" slash "test_cases.el"))
                 ;; /home/doom/tmp/perlnow_test/t65/test_set/meta/test_cases.el
          test-case-data
          cases
          )
     (test-init-load-setup-code "s65" test-set)
     (setq test-case-data (test-init-load-data test-case-file))

     (setq cases (test-init-subdirs test-set))
;;     (setq cases '("cpan1")) ;; DEBUG
;;     (setq cases '("git1")) ;; DEBUG
;;     (setq cases '("non1")) ;; DEBUG
     (setq cases (reverse cases))
      (dolist (case-name cases)
        (message "case-name: %s" case-name)
        (let ((case-data
               (test-init-plist-lookup case-name 'test-case-data))
              (case-loc
                (concat test-set case-name slash)) ;; e.g. /home/doom/tmp/perlnow_test/tXX/test_set/non1/
               start-file-list  expected-t-list  t-list-raw   t-list  t-loc-list t-loc
              )
          (message "for case: %s, case-data: %s" case-name (pp-to-string case-data)) ;; DEBUG

          ;; file paths relative to the case-loc
          (setq start-file-list
                (test-init-plist-lookup "start-file-list" 'case-data))

          ;; file names sans path
          (setq expected-t-list
                (test-init-plist-lookup "t-list" 'case-data))

          ;; start-files: list of places to try to list test files from
          (message "for case: %s, start-file-list: %s" case-name
                   (pp-to-string start-file-list)) ;; DEBUG

          (dolist (start-file start-file-list)
            (find-file (concat case-loc start-file))
            ;; the code under test
            ;; TODO instead of this, want to test perlnow-test-harder (or ?)
            ;;      then capture the generated buffer, and compare it to
            ;;      an expected version (or ?)
            (setq t-list-raw
                          (perlnow-list-perl-tests case-loc))

            (setq t-list
                  (mapcar (lambda (file) (file-name-nondirectory file))
                          (sort t-list-raw 'string<)))
            (message "XXX t-list: %s" (pp-to-string t-list))

            (assert-equal expected-t-list t-list
                          (concat test-name ": case: " case-name ": start-file " start-file))
            )
          ))
     )
   (end-tests)
   ))
