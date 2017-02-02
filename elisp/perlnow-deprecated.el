;;; perlnow-deprecated.el --- Sun  January 01, 2017  13:40

;; Copyright 2017 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: TEMPLATE.el.tpl,v 1.3 2016/10/21 17:38:51 doom Exp $
;; Keywords:
;; X-URL: not distributed yet
;; License: the same as your GNU emacs (see LICENSE below)

;;; Commentary:

;;  Code to support deprecated features, moved here from perlnow.el.
;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlnow-deprecated)

;;; Code:

(provide 'perlnow-deprecated)
(eval-when-compile
  (require 'cl))


;;;========
;;; The "alt" runstring feature.

;;; Now implementing the "alt-run-string" in addition to
;;; the "run-string": having both allows for
;;; having two separate concurrently defined ways of running the
;;; the perl code in the current buffer.  The heuristics for
;;; guessing what run string to use remain identical.
;;; (( TODO -- maybe they will become identical again someday ))

(defvar perlnow-script-alt-run-string nil
  "The alternative run string for perl scripts, used by \\[perlnow-alt-run].

  Warning: Deprecated feature.

Leave this set to nil unless you want to override the heuristics
used by \\[perlnow-set-alt-run-string] to determine the way to test
the current script.  This is a buffer local variable, i.e. it
may be set differently for different files.")
(put 'perlnow-script-alt-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-script-alt-run-string)

(defvar perlnow-module-alt-run-string nil
  "The alternative run string for perl modules, used by \\[perlnow-alt-run].

   Warning: Deprecated feature.

Leave this set to nil unless you want to override the heuristics
used by \\[perlnow-set-alt-run-string] to determine the way to test
the current script.  This is a buffer local variable, i.e. it
may be set differently for different files.")
(put 'perlnow-module-alt-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-module-alt-run-string)

(defvar perlnow-alt-run-string nil
  "Tells \\[perlnow-alt-run] how to run the code in a particular file buffer.
This is a buffer local variable which is set by  \\[perlnow-script-alt-run-string],
and this should not typically be set by the user directly.
See `perlnow-script-alt-run-string' and `perlnow-module-alt-run-string' instead.")
(put 'perlnow-alt-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-alt-run-string)


;;;==========================================================
;;; Intentionally neglected commands (use "alt" runstrings).
;;; Bumping 'em down here where I can forget about them for now.
;;; Reimplement later.
;;; Maybe: work out a way to extend the main routines
;;; to do the "alt" handling?

(defun perlnow-alt-run (alt-run-string)
  "Run the perl code in this file buffer.

   Warning: Deprecated feature.

This uses an interractively set ALT-RUN-STRING determined
from `perlnow-alt-run-string' which may have been set by using
\\[perlnow-set-alt-run-string].  If `perlnow-alt-run-string' is nil,
\\[perlnow-set-alt-run-string] is called automatically.\n
The alt run string can always be changed later by running
\\[perlnow-set-alt-run-string] manually."
  (interactive
   (let (input)
     (if (eq perlnow-alt-run-string nil)
         (setq input (perlnow-set-alt-run-string))
       (setq input perlnow-alt-run-string))
     (list input)
     ))
  (perlnow-run alt-run-string)) ; Note: uses perlnow-run rather than running compile directly


(defun perlnow-set-alt-run-string ()
  "Prompt the user for a new alternative run string for the current buffer.

  Warning: Deprecated feature.

This sets the global variable `perlnow-alt-run-string' that \\[perlnow-alt-run]
will use to run the code in future in the current buffer.
Frequently, the user will prefer to use \\[perlnow-alt-run] and let it
urun this command indirectly if need be; however using this command
directly is necessary to change the alt-run command string later.  \n
From within a program, it's probably best to set some variables
directly, see `perlnow-script-alt-run-string' and `perlnow-module-alt-run-string'.\n
This function uses \\\[perlnow-module-code-p] to see if the code looks like a
module (i.e. does it have a package line), otherwise it
assumes it's a perl script.  The heuristics for setting a default
\"alt\"-run string are identical to those used for setting the
`perlnow-run-string'."
;;; perlnow-set-alt-run-string is a minor variation of perlnow-set-run-string
  (interactive)
  (cond
   ((perlnow-module-code-p)
    ;; set-up a decent default value
    (unless perlnow-module-alt-run-string
      (progn
        (setq perlnow-module-alt-run-string
              (perlnow-guess-module-run-string))))
    ;; ask user the alternative way to run this module (use as default next time)
    (setq perlnow-module-alt-run-string
          (read-from-minibuffer
           "Set the alternative run string for this module: "
           perlnow-module-alt-run-string))
    ;; tell perlnow-alt-run how to do it
    (setq perlnow-alt-run-string perlnow-module-alt-run-string))
   (t  ;;  assume it's a script since it's not a module.
    ;; set-up intelligent default alt run string
    (unless perlnow-script-alt-run-string
      (progn
        (setq perlnow-script-alt-run-string
              (perlnow-guess-script-run-string))
        ))
    ;; ask user the alternative way to run this script (use as default next time)
    (setq perlnow-script-alt-run-string
          (read-from-minibuffer
           "Set the alternative run string for this script: "
           perlnow-script-alt-run-string))
    ;; tell perlnow-alt-run to do it that way
    (setq perlnow-alt-run-string perlnow-script-alt-run-string))))




;;;==========================================================
;;; cheat commands ("cheat" == automatically fix things so checks pass)
;;;

;; TODO complains if you run this from a non *.t buffer...
;; but if it has a *.t file associated, it should switch to there
;; and do the "revise" magic over there.
;; TODO verify that *compilation* shows results from running
;; the *.t, if not, do a run first.
(defun perlnow-revise-test-plan ()
  "Revise the test plan to match the current count of tests.

   DEPRECATED: just use done_testing()

Presumes you've just run the test, and checks the '*compilation*'
buffer to find out what the actual count of tests are, and
modifies the *.t file's plan to match."
;; Note: presumes that perlnow-run uses standard buffer name *compilation*.
  (interactive)
  (let* ((full-file (buffer-file-name))
         (location (file-name-directory full-file))
         (filename (file-name-nondirectory full-file))
         (extension (perlnow-file-extension filename))
         ;; (original-buffer (current-buffer)) ;; needed?
         (plan-count-pattern
          "Looks like you planned \\([0-9]+\\) test.*?but ran \\([0-9]+\\)")
         (planned-tests)
         (actual-tests)
         (planned-tests-pattern))
    (save-excursion
      ;; verify that we're inside a *.t buffer
      (unless (string= extension "t")
        (error "Can only run perlnow-revise-test-plan on a *.t file."))
      (save-excursion
        (set-buffer "*compilation*")
        (goto-char (point-min))
        (cond ( (re-search-forward plan-count-pattern)
                (setq planned-tests (match-string 1))
                (setq actual-tests  (match-string 2))
                )
              (t
               (message "Is the test plan already up-to-date?")))
        ) ;; return to the *.t buffer
      ;; (set-buffer original-buffer) ;; uncomment, if needed

      ;; Skip to top, search down for 'Test::',
      (goto-char (point-min))
      (cond ( (and
               (re-search-forward "Test::")
               (re-search-forward "plan") )
            ;; No good?  Why?  TODO  "\b" doesn't work either
            ;;  (setq planned-testes-pattern (format "\\b" planned-tests "\\b" ))
              (setq planned-testes-pattern planned-tests)
               (cond ((re-search-forward planned-testes-pattern nil t)
                       (replace-match actual-tests nil t))
                     (t
                      (message "Failed to find expected %s after \"plan\"." planned-tests)
                      )))
            (t
             (message "Test::More plan not found."))
            ))))



;;;=======
;;; general snippets


;; (defvar perlnow-screen-height 60
;; "A hack to deal with the fact that I don't know where the function
;; 'screen-height' that I used to use has gone.")
;; Now using frame-height.






;; LICENSE

;; This program is free software; you can redistribute it and/or modify
;; it under the same terms as the version of GNU Emacs you intend to use it with.

;; At present, GNU Emacs is under the GNU General Public License version 3
;; or (at your option) any later version.  This license is as published by
;; the Free Software Foundation.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; perlnow-deprecated.el ends here
