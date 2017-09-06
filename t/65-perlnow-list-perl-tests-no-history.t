#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   June 18, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/elisp/t/68-perlnow-test-harder-select-menu.t
;; Mutated from: 65-perlnow-list-perl-tests-no-history.t

;; This test reads in the file dat/code/s65/meta/test_cases.el, and uses that 
;; to drive the tests.  As of this writing, that file contains three main
;; cases "cpan1", "git1" and "non1", which have corresponding directory trees
;; in dat/code/s65.  These trees contain dummy perl dev environments that 
;; have been set-up in advance, so we step into them without knowing anything
;; about how they were created (we have "no history" for them).
;; Each test case has a "start-file-list", a list of files we're going to
;; open in turn before running perlnow-list-perl-tests.  We call it a pass 
;; when  the result matches the expected one supplied in the "t-list" field of 
;; the test cases.

;; The major cases:
;;   cpan1 is a cpan-style structure (created with milla).
;;   git1  is a git-controlled project
;;   non1  is a non-cpan/non-git project (the hardest case)

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
     (setq cases (test-init-subdirs test-set)) ;; cpan1 git1 non1
     ;; TODO can set cases to a list of one for debugging...
     (setq cases (reverse cases))
      (dolist (case-name cases)
        (if perlnow-debug
            (message "case-name: %s" case-name))
        (let ((case-data
               (test-init-plist-lookup case-name 'test-case-data))
              (case-loc
                (concat test-set case-name slash)) ;; e.g. /home/doom/tmp/perlnow_test/tXX/test_set/non1/
               start-file-list  expected-t-list  t-list-raw   t-list  t-loc-list t-loc
               full-start-file
              )
          (if perlnow-debug
              (message "for case: %s, case-data: %s" case-name (pp-to-string case-data))) 
          ;; file paths relative to the case-loc
          (setq start-file-list (test-init-plist-lookup "start-file-list" 'case-data))
          ;; adding start-from-directory cases
          (push (perlnow-one-up (car start-file-list))  start-file-list)
          (push (perlnow-one-up (nth 3 start-file-list)) start-file-list)

          ;; file names sans path
          (setq expected-t-list (test-init-plist-lookup "t-list" 'case-data))
          ;; start-files: list of places to try to list test files from
          (if perlnow-debug
              (message "for case: %s, start-file-list: %s" case-name
                       (pp-to-string start-file-list)))

          (dolist (start-file start-file-list)
            (message "case: %s start-file: %s" case-name start-file) ;; DEBUG
            (setq full-start-file (concat case-loc start-file))
            ;; (find-file full-start-file)  ;; if this isn't open makes things harder on the non1 case

            ;; and at last we're down to calling the code under test...

            ;; TODO instead of this,  (( old comment, barely get what it's getting at-- September 05, 2017  ))

            ;;      want to test perlnow-test-harder (or ?)
            ;;      then capture the generated buffer, and compare it to
            ;;      an expected version (or ?)

            (setq t-list-raw (perlnow-list-perl-tests full-start-file))

            (setq t-list
                  (mapcar (lambda (file) (file-name-nondirectory file))
                          (sort t-list-raw 'string<)))
            (if perlnow-debug
                (message "XXX t-list: %s" (pp-to-string t-list)))
            (assert-equal expected-t-list t-list
                          (concat test-name ": case: " case-name ": start-file " start-file))
            )
          ))
     )
   (end-tests)
   ))
