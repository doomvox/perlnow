#!/usr/local/bin/emacs --script
;; #! /usr/bin/emacs --script
;;                                                   June 18, 2017
;; ~/End/Cave/Perlnow/lib/perlnow/t/68-perlnow-test-harder-select-menu.t
;; Mutated from: 63-perlnow-list-perl-tests-lone-module.t

;; See discussion at top of: 62-perlnow-list-perl-tests-cpan-style.t

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
          (dev-location (concat test-loc "dev"))
          (test-set        (concat test-loc "test_set" slash))
          (test-case-file  (concat test-set "meta" slash "test_cases.el"))
                 ;; /home/doom/tmp/perlnow_test/t68/test_set/meta/test_cases.el

          (select-buffer-text-file (concat test-loc "scraped-buffer-text.txt"))

          (test-select-menu-file-key-base "test-select-menu-file")
          (test-select-menu-file-key test-select-menu-file-key-base)

          test-select-menu-file-key
          test-case-data
          cases
          )
     (test-init-load-setup-code "s68" test-set)
     (setq test-case-data (test-init-load-data test-case-file))

     (setq cases  (reverse (test-init-subdirs test-set)))
     (dolist (case-name cases)
        (message "case-name: %s" case-name)
        (let ((case-data
               (test-init-plist-lookup case-name 'test-case-data))
              (case-loc
                (concat test-set case-name slash)) ;; /home/doom/tmp/perlnow_test/tXX/test_set/non1/
               start-file-list  expected-t-list  t-list-raw   t-list  t-loc-list t-loc
               expected-select-buffer-text-file
               expected-select-buffer-text

              )
          (message "for case: %s, case-data: %s" case-name (pp-to-string case-data)) ;; DEBUG

          ;; file paths relative to the case-loc
          (setq start-file-list
                (test-init-plist-lookup "start-file-list" 'case-data))

          ;; these file names are *sans path* ((TODO Rather have relative to "t", no?))
          (setq expected-t-list
                (test-init-plist-lookup "t-list" 'case-data))

          ;; start-files: list of places to try to list test files from
          (message "for case: %s, start-file-list: %s" case-name
                   (pp-to-string start-file-list)) ;; DEBUG

          (setq expected-select-buffer-text-file
                (concat test-set "meta" slash
                        (car (test-init-plist-lookup "test-select-menu-file" 'case-data))))

          (find-file expected-select-buffer-text-file)

          (setq expected-select-buffer-text
                (buffer-substring-no-properties (point-min) (point-max)))

          (dolist (start-file start-file-list)

            (find-file (concat case-loc start-file))
            ;; redundant with 65-*.t
;;             (setq t-list-raw
;;                           (perlnow-list-perl-tests case-loc))
;;             (setq t-list
;;                   (mapcar (lambda (file) (file-name-nondirectory file))
;;                           (sort t-list-raw 'string<)))
;;             (message "XXX t-list: %s" (pp-to-string t-list))

;;             (let* ((test-name "Testing perlnow-list-perl-tests"))
;;               (assert-equal expected-t-list t-list
;;                             (concat test-name ": case: " case-name ": start-file " start-file)))


            ;; the real test
            (let* ((harder-setting 4))
              (perlnow-edit-test-file-harder harder-setting))
            (let* ((select-buffer-text
                    (buffer-substring-no-properties (point-min) (point-max)))
                   (test-name "perlnow-edit-test-file-harder")
                   (fail-message (concat test-name ": case: " case-name ": start-file " start-file))
                   status
                   )
              (assert-equal expected-select-buffer-text select-buffer-text fail-message)

              (find-file select-buffer-text-file)
              (delete-region (point-min) (point-max))
              (insert select-buffer-text)
              (basic-save-buffer)
               (message "file from scraped buffer: %s" select-buffer-text-file)
;;               ;; /home/doom/tmp/perlnow_test/t68/scraped-buffer-text.txt
               (message "compare to expected: %s" expected-select-buffer-text-file)
              ))
          ))
     )
   (end-tests)
   ))
