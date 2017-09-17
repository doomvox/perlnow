#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                 September 06, 2017  
;; /home/doom/End/Cave/Perlnow/lib/perlnow/t/01-perlnow-misc.t
;;
;; Test story: 
;;   There are a bunch of utilities scattered throughout the perlnow code 
;;   with embedded, commented out trial run invocations.  I want to clean
;;   them up, so I'm turning them into real tests.

;; First I copy over a tree full of files to run on, copying s01 to t01
;; using the test-init-load-setup-code function.

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
    (perlnow-tron)

   ;; debugging convenience hack
   (let ((my-dev "/home/doom/End/Cave/Perlnow/lib/perlnow/t/"))
     (if (file-accessible-directory-p my-dev)
         (cd my-dev)))

   (let* (
          (slash test-init-slash)
          (test-loc (test-init))
          (dev-location    (concat test-loc "dev"))
          (test-set        (concat test-loc "test_set" slash))
          (test-case-file  (concat test-set "meta" slash "test_cases.el"))
                 ;; /home/doom/tmp/perlnow_test/t01/test_set/meta/test_cases.el
          test-case-data  cases
          )
     (test-init-load-setup-code "s01" test-set)

     (message "test-loc: %s" test-loc) ;; /home/doom/tmp/perlnow_test/t01/
     (message "test-set: %s" test-set) ;; /home/doom/tmp/perlnow_test/t01/test_set/
       ;; bin  cpan1  git1  lib  meta  nestnon1  nestnon2  non1  t  

     (let* (
           (target-function "perlnow-dirs")
           (test-mess (format "Testing %s " target-function))
           (loc1a (concat test-set "git1/lib"))
           (loc1b (concat test-set "git1/lib" slash))

           (exp1-raw (list "Math" "Poetx"))  
           (exp1 (mapcar '(lambda (name) (concat loc1b name)) exp1-raw))

           (loc2 (concat test-set "mix" slash))
           (exp2-raw (list "sub2" "sub1" "submission"))
           (exp2 (mapcar '(lambda (name) (concat loc2 name))
                         (sort exp2-raw 'string<)
                         ))

           (loc3 nil)
           (exp3 nil)

           (loc4 "")
           (exp4 nil)

           result )
       (setq result (sort (perlnow-dirs loc1a) 'string<))
       (assert-equal exp1 result (concat test-mess ": path without trailing slash"))

       (setq result (sort (perlnow-dirs loc1b) 'string<))
       (assert-equal exp1 result (concat test-mess ": path with trailing slash"))

       (setq result (sort (perlnow-dirs loc2) 'string<))
       (if perlnow-debug (message "loc2: %s" loc2))
       (if perlnow-debug
           (dolist (item result) (message "loc2 item: %s" item)))
         
       (assert-equal exp2 result (concat test-mess ": pick out dirs from mix with files"))

       (setq result (perlnow-dirs loc3))
       (assert-equal exp3 result (concat test-mess ": listing of nowhere is nothing"))

       (setq result (perlnow-dirs loc4))
       (assert-equal exp4 result (concat test-mess ": listing of nothing is nothing"))
       ) ;; end perlnow-dirs

     (let* (
           (target-function "perlnow-one-up")
           (test-mess (format "Testing %s " target-function))

           (loc1a (concat test-set "git1/lib"))
           (loc1b (concat test-set "git1/lib" slash))

           (exp1 (concat test-set "git1" slash))

           (loc2 (concat test-set "mix" slash))
           (exp2 test-set)
                 
           (loc3 nil)
           (exp3 nil)

           (loc4 "")
           (exp4 nil)

           (case5 "odd case: no slashes yields nil")
           (loc5 "yabbas") 
           (exp5 nil)
           
           (case6 "relative path")
           (loc6 "dev/Radioactive/Bytes.pm") 
           (exp6 "dev/Radioactive/")

           result )
       (setq result (perlnow-one-up loc1a))
       (assert-equal exp1 result (concat test-mess ": path without trailing slash"))

       (setq result (perlnow-one-up loc1b))
       (assert-equal exp1 result (concat test-mess ": path with trailing slash"))

       (setq result (perlnow-one-up loc2))
       (message "loc2: %s" loc2)
       (assert-equal exp2 result (concat test-mess ": another path with trailing slash"))

       (setq result (perlnow-one-up loc3))
       (assert-equal exp3 result (concat test-mess ": listing of nowhere is nothing"))

       (setq result (perlnow-one-up loc4))
       (assert-equal exp4 result (concat test-mess ": listing of nothing is nothing"))

       (setq result (perlnow-one-up loc5))
       (assert-equal exp5 result (concat test-mess ": " case5))

       (setq result (perlnow-one-up loc6))
       (assert-equal exp6 result (concat test-mess ": " case6))

       ) ;; end perlnow-one-up


     (let* (
           (target-function "perlnow-path-tail")
           (test-mess (format "Testing %s " target-function))

           (loc1a (concat test-set "git1/lib"))
           (loc1b (concat test-set "git1/lib" slash))
           (exp1 "lib")

           (loc2a (concat test-set "mix" ))
           (loc2b (concat test-set "mix" slash))
           (exp2 "mix")
                 
           (loc3 nil)
           (exp3 nil)

           (loc4 "")
           (exp4 "") ;; okay?

           (case5 "odd case: no slashes, no change")
           (loc5 "yabbas") 
           (exp5 "yabbas")
           
           (case6 "relative path")
           (loc6 "dev/Radioactive/Bytes.pm") 
           (exp6 "Bytes.pm")

           result )
       (setq result (perlnow-path-tail loc1a))
       (assert-equal exp1 result (concat test-mess ": path without trailing slash"))

       (setq result (perlnow-path-tail loc1b))
       (assert-equal exp1 result (concat test-mess ": path with trailing slash"))

       (setq result (perlnow-path-tail loc2a))
       (message "loc2a: %s" loc2a)
       (assert-equal exp2 result (concat test-mess ": another path without trailing slash"))

       (setq result (perlnow-path-tail loc2b))
       (message "loc2b: %s" loc2b)
       (assert-equal exp2 result (concat test-mess ": another path with trailing slash"))

        (setq result (perlnow-path-tail loc3))
        (assert-equal exp3 result (concat test-mess ": given nothing, get nothing"))

        (setq result (perlnow-path-tail loc4))
        (assert-equal exp4 result (concat test-mess ": given blank, get nothing"))

        (setq result (perlnow-path-tail loc5))
        (assert-equal exp5 result (concat test-mess ": " case5))

        (setq result (perlnow-path-tail loc6))
        (assert-equal exp6 result (concat test-mess ": " case6))
       ) ;; end perlnow-path-tail

     (let* (
           (target-function "perlnow-stepup-for-pm-loc")
           (test-mess (format "Testing %s " target-function))

           (case1 ": from a test file")
           (loc1 (concat test-set "et_fa/t/01-yoda.t"))
           (mod1 "Bongos::Doom")
           (exp1 (concat test-set "et_fa/lib/"))

           (case2 ": from the test location")
           (loc2 (concat test-set "et_fa/t/"))
           (mod2 "Bongos::Doom")
           (exp2 (concat test-set "et_fa/lib/"))
                 
           (case3 ": from inside the lib tree")
           (loc3 (concat test-set "et_fa/lib/Bongos" slash))
           (mod3 "Bongos::Doom")
           (exp3 (concat test-set "et_fa/lib/"))

           (case4a ": unusual lib location name, not found")
           (case4b ": unusual lib location name, found if lib-location-names primed")
           (loc4 (concat test-set "nestnon1/t/pillar" slash))
           (mod4 "Marble::Pillar")
           (exp4a nil)
           (exp4b (concat test-set "nestnon1/dev/"))  ;; after (push "dev" perlnow-lib-location-names)
           )
       (setq result (perlnow-stepup-for-pm-loc mod1 loc1))
       (assert-equal exp1 result (concat test-mess case1))

       (setq result (perlnow-stepup-for-pm-loc mod2 loc2))
       (assert-equal exp2 result (concat test-mess case2))

       (setq result (perlnow-stepup-for-pm-loc mod3 loc3))
       (assert-equal exp3 result (concat test-mess case3))

       (setq result (perlnow-stepup-for-pm-loc mod4 loc4))
       (assert-equal exp4a result (concat test-mess case4a))

       (push "dev" perlnow-lib-location-names)

       (setq result (perlnow-stepup-for-pm-loc mod4 loc4))
       (assert-equal exp4b result (concat test-mess case4b))

       ) ;; end perlnow-stepup-for-pm-loc
     ) ;; end outer let 
   (end-tests)
   ))


