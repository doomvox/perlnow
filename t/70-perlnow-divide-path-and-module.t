#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                             October 02, 2017  
;; /home/doom/End/Cave/Perlnow/lib/perlnow/t/70-perlnow-divide-path-and-module.t
;;
;; Test story: 
;;   Testing two routines that split a full path into path and remainder.
;;   
;; These are very similar routines that differ on whether the colon seperated 
;; portion should be part of the path or the remainder.

;; colons part of path:
;;   perlnow-divide-module-path-dir-and-tail
;; colons part of name:
;;   perlnow-divide-hybrid-path-and-package-name

;;   I'm about to re-write these routines for portability
;;   removing an old unix-dependency: regexps that assume slash
;;   as file-system seperator.

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
    (perlnow-tron)

   ;; debugging convenience hack
    (let ((my-dev "/home/doom/End/Cave/Perlnow/lib/perlnow/t/"))
      (if (file-accessible-directory-p my-dev)
          (cd my-dev)) )

    (let* (
           (slash test-init-slash)
           (test-loc (test-init))
           (target-function-1 "perlnow-divide-module-path-dir-and-tail")
           (target-function-2 "perlnow-divide-hybrid-path-and-package-name")
           )

      (let* (
             (test-mess-1 (format "Testing %s: " target-function-1))
             (test-mess-2 (format "Testing %s: " target-function-2))
             (input "/home/doom/lib/Striated")
             (expected-path "/home/doom/lib/") 
             (expected-name "Striated") 
             result-pair  path   name
             )
        (setq result-pair
              (funcall (intern target-function-1) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name name (concat test-mess-1 "name from " input ))
        (assert-equal expected-path path (concat test-mess-1 "path from " input ))

        (setq result-pair
              (funcall (intern target-function-2) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name name (concat test-mess-2 "name from " input ))
        (assert-equal expected-path path (concat test-mess-2 "path from " input ))
        )

      (let* ((test-mess-1 (format "Testing %s: " target-function-1))
             (test-mess-2 (format "Testing %s: " target-function-2))
             (input "/home/doom/lib/Taxi::Tude")
             (expected-path-1 "/home/doom/lib/Taxi::")
             (expected-name-1 "Tude") 
             (expected-path-2 "/home/doom/lib/")
             (expected-name-2 "Taxi::Tude") 
             result-pair  path   name
             )
        (setq result-pair
              (funcall (intern target-function-1) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-1 name (concat test-mess-1 "name from " input ))
        (assert-equal expected-path-1 path (concat test-mess-1 "path from " input ))

        (setq result-pair
              (funcall (intern target-function-2) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-2 name (concat test-mess-2 "name from " input ))
        (assert-equal expected-path-2 path (concat test-mess-2 "path from " input ))
        )

      ;; (perlnow-divide-module-path-dir-and-tail "/tmp/Fing::Fang:")
      (let* ((test-mess-1 (format "Testing %s: " target-function-1))
             (test-mess-2 (format "Testing %s: " target-function-2))
             (input "/tmp/Fing::Fang:")
;; This is existing behavior:
;;              (expected-path-1 nil)
;;              (expected-name-1 nil) 
;; After revisions, expect this sort of behavior:
             (expected-path-1 "/tmp/Fing::Fang:")
             (expected-name-1 "") 

             (expected-path-2 "/tmp/")
             (expected-name-2 "Fing::Fang:") 
             result-pair  path   name
             )
        (setq result-pair
              (funcall (intern target-function-1) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-1 name (concat test-mess-1 "name from " input ))
        (assert-equal expected-path-1 path (concat test-mess-1 "path from " input ))

        (setq result-pair
              (funcall (intern target-function-2) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-2 name (concat test-mess-2 "name from " input ))
        (assert-equal expected-path-2 path (concat test-mess-2 "path from " input ))
        )

      (let* ((test-mess-1 (format "Testing %s: " target-function-1))
             (test-mess-2 (format "Testing %s: " target-function-2))
             (input "/tmp/Fing::Fang::")
             (expected-path-1 "/tmp/Fing::Fang::")
             (expected-name-1 "") 
             (expected-path-2 "/tmp/")
             (expected-name-2 "Fing::Fang::") 
             result-pair  path   name
             )
        (setq result-pair
              (funcall (intern target-function-1) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-1 name (concat test-mess-1 "name from " input ))
        (assert-equal expected-path-1 path (concat test-mess-1 "path from " input ))

        (setq result-pair
              (funcall (intern target-function-2) input))
        (setq path (nth 0 result-pair))
        (setq name (nth 1 result-pair))

        (assert-equal expected-name-2 name (concat test-mess-2 "name from " input ))
        (assert-equal expected-path-2 path (concat test-mess-2 "path from " input ))
        )

      ) ;; end outer let 
    (end-tests)
    ))
