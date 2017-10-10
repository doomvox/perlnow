#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                             October 08, 2017  
;; /home/doom/End/Cave/Perlnow/lib/perlnow/t/72-perlnow-choose-template.t
;;
;; Test story: 
;;   Run the routines such as perlnow-choose-module-template with different
;;   sets of templates.
;; 
;;  (perlnow-choose-module-template template-tag module-style template-location)
;;

(defun perlnow-test-72-core (target-function cpan-style module-style expected-template-name setup-label)
  "Tests the given TARGET-FUNCTION with the given arguments."
  (let ( test-label test-message template-full  template-name )
    (setq test-label (format "Testing %-35s" (concat target-function ": ")))
    (setq test-message (concat test-label
                               (format "setup %s pick for %s %s "
                                       setup-label cpan-style module-style )))
    (setq template-full
          (funcall (intern target-function)
                   cpan-style module-style template-location))
    (setq template-name (file-name-nondirectory template-full))
    (assert-equal expected-template-name template-name test-message)
    ))

(funcall
 (lambda ()
   (if (file-exists-p "test-init.el")
       (load-file "test-init.el"))
   (perlnow-tron)
   ;; debugging convenience hack
   (let ((my-dev "/home/doom/End/Cave/Perlnow/lib/perlnow/t/"))
     (if (file-accessible-directory-p my-dev)
         (cd my-dev)))

   ;; outermost let, run once per *.t file
   (let* ((slash test-init-slash)
          (test-loc     (test-init))  ;; /home/doom/tmp/perlnow_test/t72/
          (test-set-loc (concat test-loc "test_set" slash)) )
     (test-init-load-setup-code "s72" test-set-loc)
     ;; group of sub-tests with one setup
     (let* ((setup-label "alpha") ;; with templates similar to my current ones
            (case-loc-name (format "templates_%s" setup-label))
            (template-location (concat test-set-loc case-loc-name slash))
            )
       ;; intermediate blocks: groups of tests for a function
       (let* ((target-function "perlnow-choose-module-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                (expected-template-name "TEMPLATE.perlnow-milla-object-pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.perlnow-pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         ) 
       (let* ((target-function "perlnow-choose-module-t-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                (expected-template-name "TEMPLATE.perlnow-milla-object-pm-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pm-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.perlnow-pm-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pm-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         ) 

       (let* ((target-function "perlnow-choose-script-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                ;; (expected-template-name "TEMPLATE.perlnow-milla-object-pl.tpl")
                (expected-template-name "TEMPLATE.perlnow-pl.tpl")
                )
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.perlnow-pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         )

       (let* ((target-function "perlnow-choose-script-t-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                ;; (expected-template-name "TEMPLATE.perlnow-milla-object-pl.tpl")
                (expected-template-name "TEMPLATE.perlnow-pl-t.tpl")
                )
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.perlnow-pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.perlnow-pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         )
       ) ;; end alpha setup

     ;; Huh?
     ;;   cd("/home/doom/End/Cave/Perlnow/lib/perlnow/t/dat/code/s72/dat/code/s72/")


     ;; outer-let for a group of sub-tests with one setup
     (let* ((setup-label "beta")  ;; with a minimal set of three templates
            (case-loc-name (format "templates_%s" setup-label))
            (template-location (concat test-set-loc case-loc-name slash))
            )
       ;; intermediate blocks: groups of tests for a function
       (let* ((target-function "perlnow-choose-module-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                (expected-template-name "TEMPLATE.pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.pm.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         ) 
       (let* ((target-function "perlnow-choose-module-t-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                (expected-template-name "TEMPLATE.t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         ) 

       (let* ((target-function "perlnow-choose-script-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                (expected-template-name "TEMPLATE.pl.tpl")
                )
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.pl.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         )
       (let* ((target-function "perlnow-choose-script-t-template"))
         ;; now, into the individual tests
         (let* ((cpan-style "milla")
                (module-style "object")
                ;; (expected-template-name "TEMPLATE.perlnow-milla-object-pl.tpl")
                (expected-template-name "TEMPLATE.pl-t.tpl")
                )
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style nil)
                (expected-template-name "TEMPLATE.pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style nil)
                (module-style "exporter")
                (expected-template-name "TEMPLATE.pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         (let* ((cpan-style "modstar")
                (module-style nil)
                (expected-template-name "TEMPLATE.pl-t.tpl"))
           (perlnow-test-72-core target-function cpan-style module-style expected-template-name setup-label))
         )


       ) ;; end beta setup 

     (end-tests)
     )))
