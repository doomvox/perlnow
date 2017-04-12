#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 18-perlnow-stash-lookup.el

;; Motivation:
;;   Early versions of perlnow-stash-put and perlnow-stash-lookup were sloppy
;;   about handling a specific given plist, and tended to always use the global
;;   default.  Here we verify that either works as it should.

;; The test story:
;;   Use perlnow-stash-put with a local plist, adding several key-value pairs
;;   Use perlnow-stash-lookup to extract one of the values.
;;   Verify global default plist stash is untouched: perlnow-incspot-from-t-plist
;;   Extract values from local plist directly with list manipulation features.
;;   Run some "put"s and "lookup"s using the global default.

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   ;; (perlnow-tron)
   (let* (
          (test-loc (test-init))

          (funcname "perlnow-stash-lookup")
          (test-name (concat "Testing " funcname ))

          ;; These aren't used as actual file-paths, no need to make portable
          (key1 "/home/doom/tmp/wooden_stake.t")
          (val1 "/home/doom/tmp/lib/Republican/Candy/Dates.pm")

          (key2 "/home/doom/tmp/silver_cross.t")
          (val2 "/home/doom/tmp/lib/Republican/Ops.pm")

          (key3 "/home/doom/tmp/dev_x/autojack.t")
          (val3 "/home/doom/tmp/dev_x/lib/Racer/Geis.pm")

          (mah-plist ())
          check-val expected2
          )

     (perlnow-stash-put key1 val1 'mah-plist )
     (perlnow-stash-put key2 val2 'mah-plist )
     (perlnow-stash-put key3 val3 'mah-plist )

     (setq check-val
           (perlnow-stash-lookup key2 'mah-plist ))
     (setq expected2 val2)

     (assert-equal expected2 check-val
                   (concat test-name ":looked up one of three vals"))

     (let* ( (funcname "perlnow-stash-put")
             (test-name (concat "Testing " funcname ))

             check-val1 check-val2
             )

       (if perlnow-debug
           (message "mah-plist: %s\n" (pp mah-plist)))

       (if perlnow-debug
           (message "perlnow-incspot-from-t-plist: %s\n" (pp perlnow-incspot-from-t-plist)))

       (assert-nil
        perlnow-incspot-from-t-plist
        (concat test-name ": the global stash plist should still be empty"))

       (setq check-val1 (nth 1 mah-plist)) ;; ... Republican/Candy/Dates.pm
       (setq check-val2 (nth 3 mah-plist)) ;; ... Republican/Ops.pm
       (assert-equal val1 check-val1 (concat test-name ": val1 as expected"))
       (assert-equal val2 check-val2 (concat test-name ": val2 as expected"))
       )

     (let* ( (funcname "perlnow-stash-put")
             (test-name (concat "Testing " funcname ))
             val-alpha val-beta val-gamma
             length-global-plist
             )
       (perlnow-stash-put "red"   "shoes")
       (perlnow-stash-put "fade"  "away")
       (perlnow-stash-put "funky" "butt")
       (perlnow-stash-put "milk"  "float")
       (perlnow-stash-put "louie" "louie")

       (message "perlnow-incspot-from-t-plist: %s" (pp perlnow-incspot-from-t-plist))
       (setq length-global-plist
             (length perlnow-incspot-from-t-plist))

       (assert-equal 10 length-global-plist
                     (concat test-name ": confirm global plist stash is default"))

;;        (nth 2 perlnow-incspot-from-t-plist)  ;; shoes
;;        (nth 4 perlnow-incspot-from-t-plist)  ;; away
;;        (nth 6 perlnow-incspot-from-t-plist)  ;; butt
;;        (nth 8 perlnow-incspot-from-t-plist)  ;; float
;;        (nth 10 perlnow-incspot-from-t-plist) ;; louie
       )

     (setq val-alpha
           (perlnow-stash-lookup "fade"))
     (setq val-beta
           (perlnow-stash-lookup "red"))
     (setq val-gamma
           (perlnow-stash-lookup "louie"))

     (assert-equal "away"  val-alpha
                   (concat test-name ": Looked up middle value, default stash"))
     (assert-equal "shoes" val-beta
                   (concat test-name ": Looked up first value,  default stash"))
     (assert-equal "louie" val-gamma
                   (concat test-name ": Looked up first value,  default stash"))

     )
   ;; ...
    (end-tests)))
