#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;; 19-perlnow-tloc-lib-stash.t

;; Motivation:

;;   Experimenting with the idea of making the incspot-from-t-plist
;;   persistant via a json file.

;;    /home/doom/.emacs.d/perlnow/incspot_from_t.json

;; The test story:
;;   Use perlnow-stash-put with the global plist, adding several key-value pairs
;;   Check the json file to see if the keys and values are accumulating
;;   Erase the global default plist stash var: perlnow-incspot-from-t-plist
;;   Run a reload function to restore the global stash from disk: perlnow-stash-reload
;;   Extract values from global plist with perlnow-stash-lookup

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
   (perlnow-tron)
   ;; Erase the global default plist stash var: perlnow-incspot-from-t-plist
   (setq perlnow-incspot-from-t-plist ())
   (let* (
          (test-loc (test-init))

          (funcname "incspot_from_t json stash")
          (test-name (concat "Testing " funcname ))

          ;; These aren't used as actual file-paths, no need to make portable
          (key1 "/home/doom/tmp/wooden_stake.t")
          (val1 "/home/doom/tmp/lib/Republican/Candy/Dates.pm")

          (key2 "/home/doom/tmp/silver_cross.t")
          (val2 "/home/doom/tmp/lib/Republican/Ops.pm")

          (key3 "/home/doom/tmp/dev_x/autojack.t")
          (val3 "/home/doom/tmp/dev_x/lib/Racer/Geis.pm")

          (slash test-init-slash)

          json-buffer
          )

;;      (let* (()
;;             )

;;        (setq input-data-fixed (perlnow-plist-keys-string-to-symbol input-data)) ;; TODO
;;              )


     ;; The standard stash file:
     ;;   ~/.emacs.d/perlnow/incspot_from_t.json
     (if perlnow-debug
         (message "perlnow-incspot-from-t-json-file: %s" perlnow-incspot-from-t-json-file))

     ;; Use perlnow-stash-put with the global plist, adding several key-value pairs
     (perlnow-stash-put key1 val1)
     (perlnow-stash-put key2 val2)
     (perlnow-stash-put key3 val3)

     (if perlnow-debug
         (message "perlnow-incspot-from-t-plist: %s" (pp-to-string perlnow-incspot-from-t-plist)))

     (let ( check-val   expected2 )
       (setq check-val
             (perlnow-stash-lookup key2))
       (setq expected2 val2)
       (assert-equal expected2 check-val
                     (concat test-name ": looked up one of three vals"))
       )

     ;; Check the json file to see if the keys and values are accumulating
     (find-file perlnow-incspot-from-t-json-file)
     (setq json-buffer (current-buffer))
     (goto-char (point-min))
     (assert-t
      (re-search-forward key2 nil t)
          (concat test-name ": found key2 in json file"))
     (goto-char (point-min))
     (assert-t
      (re-search-forward val3 nil t)
          (concat test-name ": found val3 in json file"))
     (kill-buffer json-buffer)

     ;; Erase the global default stash
     (setq perlnow-incspot-from-t-plist ())

     ;; verify stash is empty
     (setq check-val (perlnow-stash-lookup key1))
     (assert-nil
      check-val
      (concat test-name ": Verified manually erased plist stash."))

     ;; Run reload function to restore the global stash from disk
     (perlnow-stash-reload)

     ;; Extract values from global plist with perlnow-stash-lookup
     (let ( check-val
            (expected1 val1)
            (expected2 val2)
            (expected3 val3)
            )
       (setq check-val (perlnow-stash-lookup key1))
       (assert-equal expected1 check-val
                     (concat test-name ":looked up val1"))
       (setq check-val (perlnow-stash-lookup key2))
       (assert-equal expected2 check-val
                     (concat test-name ":looked up val2"))
       (setq check-val (perlnow-stash-lookup key3))
       (assert-equal expected3 check-val
                     (concat test-name ":looked up val3"))
       )

     (setq check-val
           (perlnow-stash-lookup key2))
     (setq expected2 val2)
     (assert-equal expected2 check-val
                   (concat test-name ":looked up one of three vals"))

     (end-tests))))
