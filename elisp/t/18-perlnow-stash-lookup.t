#!/usr/local/bin/emacs --script
;; /usr/bin/emacs
;; 18-perlnow-stash-lookup.el

;; The test story:

;;
;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see below)

(funcall
 (lambda ()
   (if (file-exists-p "test-init-elisp.el")
       (load-file "test-init-elisp.el"))

   ;; meta-project, test-simple.el eval/dev: using a modified test-simple.el
   (load-file "/home/doom/End/Sys/Emacs/emacs-test-simple/test-simple.el")
   (perlnow-tron)
   (let* (
          (test-loc (test-init))

          (funcname "perlnow-stash-lookup")
          (test-name (concat "Testing " funcname ))

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

     (if perlnow-debug
         (message "mah-plist: %s\n" (pp mah-plist)))

     (if perlnow-debug
         (message "perlnow-incpot-from-t-plist: %s\n" (pp perlnow-incpot-from-t-plist)))

     (setq check-val
           (perlnow-stash-lookup key2 'mah-plist ))
     (setq expected2 val2)

     (assert-equal expected2 check-val
                   (concat test-name ":looked up one of three vals"))

     )
   ;; ...
    (end-tests)))
