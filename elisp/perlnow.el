;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.73 2004/02/13 06:14:36 doom Exp root $
;; Keywords: 
;; X-URL: http://www.grin.net/~mirthless/perlnow/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;==========================================================
;;; Commentary:

; This package is intended to make it easier to jump into the
; development of perl code when an idea strikes.

; The main idea is to automate the routine tasks involved with 
; starting work on new perl code.  A single perlnow
; command will typically prompt for a location and a name,
; open a file buffer with an appropriate framework already
; inserted (e.g. the hash-bang line, comments including date
; and author information, a perldoc outline, and so on). In
; the case of scripts the file automatically becomes executable.

; To function properly, perlnow.el requires that template.el has been 
; installed, along with two templates for perl development 
; purposes, one for scripts, another for modules.  Most likely
; ~/.templates is the place these copies of these templates should 
; be installed.

; Primarily, perlnow.el provides the following interactive
; functions:

; perlnow-script - for creation of new perl scripts. 

; perlnow-module - for creation of new moduls. 

; perlnow-script-using-this-module - for creation of a script 
;            that uses the module open in the current buffer. 

; perlnow-run-check - does a perl syntax check on the current
;            buffer, displaying error messages and warnings in  
;            the standard emacs style, so that the next-error 
;            command, usually bound to "C-x `" (control-x back-apostrophe)
;            will skip you to the location of the problem. 

; perlnow-run - like the above, except that it actually runs 
;            the code, prompting the user for a run string for 
;            it if it has not been defined yet. 

; perlnow-set-run-string - Allows the user to manually change 
;            the run-string used by perlnow-run.

; perlnow-h2xs - Creates a new h2xs module structure (which is 
;            used by CPAN modules.)



;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlnow)
;;   (global-set-key  "\M-ps" 'perlnow-script)
;;   (global-set-key  "\M-pm" 'perlnow-module)
;;   (setq perlnow-script-location (substitute-in-file-name "$HOME/bin"))
;;   (setq perlnow-module-root (substitute-in-file-name "$HOME/lib"))
;; 

;;; Definitions of some terms used: 

;;; First of all, through out this documentation (and for some
;;; of the code), the simplifying assumption has been made
;;; that a perl package is a perl module is a single file,
;;; (with extension *.pm).  This works becuase even though
;;; technically multiple packages can occur in a single
;;; file, that is not the standard practice.  

;;; TODO - expand this
;;; module-file-name - the file system's name for the module file, e.g. /usr/lib/perl/Double/Colon.pm
;;; module-file-basename - name of the module file itself, sans extension: in the above example, "Colon"
;;; module-location  - directory portion of module-file-name, e.g. /usr/lib/perl/Double/
;;; module-name or package-name - perl's double colon separated name, e.g. "Double::Colon"
;;; module-root - The place where perl's double-colon name space begins (e.g. /usr/lib/perl)
;;;               Perl's @INC is a list of different module-roots. 
;;;               [silly thought: rename module-root as inc-spot?]
;;; staging-area - the directory created by the h2xs command for module development, 
;;;                a "hyphenized" form of the module-name e.g. Double-Colon.
;;;                Every staging-area contains a module-root called "lib".
;;; h2xs-location - the place where you put your staging-areas (possibly /home/doom/dev ?)

;;; Some adjectives: 
;;;    "perlish"  - means a path including double-colons (alternate term: "colon-ized"), 
;;; As opposed to: 
;;;    "file-system" (or "filesys") -  which refers to a regular slash separated path.
;;;    "full"     - means the path is included, e.g. "full-file-name".  

;;; test-script   - The *.t file associated with the current module/script(?), usually 
;;;                 something like ModuleName.t or possibly Staging-Area.t. (For a script, scriptname.t?)
;;; test-location - place where the test script(s) are for a given module/script(?)
;;; test-path     - search path to look for test files. Note, can include relative locations, e.g. "../t"


;;; Code:

(provide 'perlnow)
(eval-when-compile
  (require 'cl))

(require 'template) ; templating system: easily customizeable initial file contents



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

;;;; TODO - fix keybindings:
;;;; DONTFORGET
;;; What's the right way to do this shit:
;;; "eval" doesn't work here....

;; (defun perlnow-define-global-keys (mode-map-name)
;;    "Several key assignments made to the global key map."
;;    (define-key (eval mode-map-name) "\C-=s" 'perlnow-script)
;;    (define-key (eval mode-map-name) "\C-=m" 'perlnow-module)
;;    (define-key (eval mode-map-name) "\C-=h" 'perlnow-h2xs)
;;    (define-key (eval mode-map-name) "\C-=b" 'perlutil-perlify-this-buffer)
;;  )

;; (defun perlnow-define-perl-mode-keys (mode-map-name)
;;   "Key assignments made to the perl-mode and cperl-mode key maps."
;;   (define-key (eval mode-map-name) "\C-=c" 'perlnow-run-check)
;; )

;; (add-hook 'perl-mode-hook '(perlnow-define-perl-mode-keys))
;; (add-hook 'cperl-mode-hook '(perlnow-define-perl-mode-keys))


; TODO:
; on the following two, I'm currently using HOME environment variable for
; a default location, though it's expected this will be overriden with a
; .emacs setting.  Maybe it would be better to default to something else,
; like ~/bin and ~/lib, but in that case would have to make sure they
; exist and maybe create them otherwise. 
; Or see if they exist, and then use them, if not, silently fall back on HOME?

(defvar perlnow-script-location (getenv "HOME")
  "The default location to stash new perl scripts")
(setq perlnow-script-location (file-name-as-directory perlnow-script-location)) 

(defvar perlnow-module-root (getenv "HOME")
  "The default location to stash new perl modules")
(setq perlnow-module-root (file-name-as-directory perlnow-module-root))

(defvar perlnow-executable-setting ?\110
  "Pattern of user-group-all permission settings used when making a script executable")

(defvar perlnow-perl-script-template 
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pl.tpl")
"The template.el template new perl scripts will be created with" )
(put 'perlnow-perl-script-template 'risky-local-variable t)
;;; DEBUG only DELETE:
;;;(setq perlnow-perl-script-template 
;;;  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pl.tpl"))

(defvar perlnow-perl-module-template 
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pm.tpl")
"The template.el template new perl modules will be created with" )
(put 'perlnow-perl-module-template  'risky-local-variable t)

;;; DEBUG only DELETE:
;;;(setq perlnow-perl-module-template 
;;;  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pm.tpl"))

(defvar perlnow-perl-module-name nil
  "Used internally to pass the a module name in the perl
double-colon separated form to the template.el template for 
perl modules. ")

(defvar perlnow-module-name-history nil
"Records the minibuffer history for perl modules accessed by this package.")

;;;----------------------------------------------------------
;; Additional "expansions" for use in template.el templates.
;; 
;; PERLNOW_MODULE_NAME is for the perlnow-module function, 
;; You'll see it used in templates like so: (>>>PERLNOW_MODULE_NAME<<<);

(setq template-expansion-alist 
      (cons 
      '("PERLNOW_MODULE_NAME" (insert perlnow-perl-module-name) )
      template-expansion-alist))

;; PNFS stands for "PerlNow Filename Spaces" it should 
;; always insert the same number of spaces as characters
;; in the name of the file.  This is a gross kludge
;; which can be used to get formatting to line up, for example:
;;    (>>>FILE<<<)              (>>>AUTHOR<<<)
;;    (>>>PNFS<<<)              (>>>DATE<<<)
;; Note the utility of having "PNFS" be four characters, 
;; the same length as "FILE".  Like I said, a gross kludge.

(setq template-expansion-alist 
      (cons 
      '("PNFS" 
        (perlnow-insert-spaces-the-length-of-this-string 
         (make-string (length 
                       (file-name-nondirectory (buffer-file-name))
                       ) ?\ )
         ))
      template-expansion-alist))

; Experimenting with an alternative gross kludges 
; This moves to column 45 before inserting the user email address 
; (as understood by emacs, typically from a .emacs file setting)
; Note that this will obediently over-write anything else that might 
; already be in that area:
(setq template-expansion-alist 
      (cons 
      '("EMAIL_AT_45" ((lambda ()
                         (move-to-column 45 t)      
                         (insert user-mail-address)
                       )))
      template-expansion-alist))

; Experimenting with an alternative gross kludges 
; This moves to column 45 before inserting the timestamp 
; returned by current-time-string.
; Note that this will obediently over-write anything else that might 
; already be in that area:

(setq template-expansion-alist 
      (cons 
      '("TIMESTAMP_AT_45" ((lambda ()
                         (move-to-column 45 t)      
                         (insert (current-time-string))
                       )))
      template-expansion-alist))

; Experimental feature: should indent as though the TAB key had been hit
; suspect that you need to use this *after* code has been inserted 
; not before.
(setq template-expansion-alist 
      (cons 
      '("TAB" (indent-according-to-mode) )
      template-expansion-alist))


(defvar perlnow-minimum-perl-version "5.006"
  "Used to replace template MINIMUM_PERL_VERSON with the
minimum perl version you're planning on supporting. Note
that perl version numbers jumped from 5.006 to 5.7.0.  As of
this writing, the latest is 5.8.2")
; Defining feature MINIMUM_PERL_VERSON to insert the above as an 
; an "expansion" in a template.el template: (>>>MINIMUM_PERL_VERSON<<<);
(setq template-expansion-alist 
      (cons 
      '("MINIMUM_PERL_VERSON" (insert perlnow-minimum-perl-version))
      template-expansion-alist))

;;; DEBUG note: eval this to erase effects of the above two settings:
;;; (setq template-expansion-alist 'nil)


;;;----------------------------------------------------------
;;; I am following my instinct and using make-variable-buffer-local 
;;; to force the following to always be buffer-local, despite the
;;; admonition in the emacs lisp ref.  My reasoning: (1) this makes 
;;; the code a little simpler (I don't want to have to remember to 
;;; use make-local-variable in different places); (2) I can't think 
;;; of a case where the user would be annoyed at me depriving them 
;;; of this choice. 

(defvar perlnow-script-run-string nil 
"The run string for perl scripts, used by \[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(put 'perlnow-script-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-script-run-string)

(defvar perlnow-module-run-string nil 
"The run string for perl modules, used by \[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(put 'perlnow-module-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-module-run-string)

(defvar perlnow-run-string nil 
"A buffer local variable, set by \[perlnow-script-run-string] to tell 
\[perlnow-run] how to run the code in a particular file buffer.  This should 
not typically be set by the user directly.  See `perlnow-script-run-string' 
and `perlnow-module-run-string' instead.")
(put 'perlnow-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-run-string)

(defvar perlnow-test-path (list "." "../t" "./t")
"List of places to look for test scripts (*.t), typically these
 will look like paths specified as relative to the current 
directory via \".\" or  \"..\", though rather than the actual 
\"current\" location, they will be interpreted as relative to 
either the module root or the module location.")
(put 'perlnow-test-path  'risky-local-variable t)


;;;==========================================================
;;; User Commands
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-run-check ()
  "Run a perl check on the current buffer, displaying errors
and warnings in another window.  Afterwards, you can skip to
the location of the next problem with \\[next-error] \n 
This command is like \\[cperl-check-syntax] with one 
less prompt \(also, it does not require mode-compile.el\)"
  (interactive)
  (save-buffer)
  (setq compile-command (format "perl -cw \'%s\'" (buffer-file-name)))
  (message "compile-command: %s" compile-command)
  (compile compile-command) )
   
;;;----------------------------------------------------------
(defun perlnow-script (filename)
  "Quickly jump into development of a new perl script"
  (interactive 
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (perlnow-new-file-using-template filename perlnow-perl-script-template)
  (perlnow-change-mode-to-executable))
   
   
;;;----------------------------------------------------------
(defun perlnow-script-using-this-module (filename)
  "Quickly jump into writing a perl script that uses the module 
in the currently open buffer.  If the module is not in perl's 
search path \(@INC\), then an appropriate \"use lib\" statement 
will be added. \n
Note: if multiple packages exist in the file \\(and that's never 
really done\\) then this function will see the first package name."
  (interactive 
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (let* ( (module-filename (buffer-file-name))
          (module-location (file-name-directory module-filename))
          (package-name (perlnow-get-module-name-from-module-buffer)) 
          (module-root (perlnow-get-module-root package-name module-location))
          ) 
    (unless package-name 
      (error "%s" "This file doesn't look like a perl module (no leading package line)."))
    
    (perlnow-new-file-using-template filename perlnow-perl-script-template)
    
    ; ensure the module can be found by the script if needed, insert "use lib" line to 
    (unless (module-root-in-INC-p module-root)
      (let ((relative-path
             (file-relative-name module-filename (file-name-directory filename))
             ))
        (insert "use FindBin qw\($Bin\);\n")
        (insert "use lib \(\"$Bin/")
        (insert relative-path)
        (insert "\");\n")))
    ; insert the "use Some::Module;" line
    (insert (format "use %s;" package-name)) ;;; and maybe a qw() list? 
    (insert "\n")))
   

;;;----------------------------------------------------------
(defun perlnow-script-general (script-file-name)
  "General purpose command to quickly jump into coding a perl script. 
Looks at the current buffer and tries to guess what \"use\" lines
you might want to start coding with. If it's a perl module, or a man page 
documenting a perl module, it will give you a \"use\" line to include 
that module.  If the module is not in perl's @INC array, it will also 
insert the appropriate \"FindBin/use lib\" lines so that the script can 
find the module. If none of that applies, you just get the usual 
perl script buffer.\n
If this works well, it obviates \[perlnow-script] and 
\[perlnow-script-using-this-module].  If it doesn't they're still there."
  (interactive
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (let ( module-filename module-location package-name ) 
    (cond 
     ((setq package-name perlnow-get-module-name)
      (setq perlnow-perl-module-name package-name) ; global used to pass value into template
      (cond
          ((setq module-filename (perlnow-module-found-in-INC package-name))
            (perlnow-new-file-using-template script-file-name perlnow-perl-script-template)
             ; insert the "use Some::Module;" line
            (insert (format "use %s;\n" package-name)) ;;; and maybe a qw() list? 
           )
          ((perlnow-module-p)
            (setq module-filename (buffer-file-name))
            (perlnow-new-file-using-template script-file-name perlnow-perl-script-template)
            (setq module-location (file-name-directory module-filename))
            ; insert "use lib" line to ensure the module can be found by the script
            (let ((relative-path
                (file-relative-name module-filename (file-name-directory script-file-name))))
               (insert
                (concat "use FindBin qw\($Bin\);\n"
                        "use lib \(\"$Bin/"
                        relative-path
                        "\");\n"))
             ; insert the "use Some::Module;" line
             (insert (format "use %s;\n" package-name)) ;;; and maybe a qw() list? 
          ))
          (t
           (perlnow-script script-file-name)
           ))))))

;;;----------------------------------------------------------
(defun perlnow-module-found-in-INC (module-name) 
  "Given a perl module name \(double-colon separated form\) 
return the first module file location found in perl's @INC 
array, or nil if it is not found."
;;; TODO That "|||" shit is cheesy.  Use "\t" or something, huh?
;  (interactive "sGimme:") ; DEBUG only DELETE
  (let* (  full return
           (module-file-tail 
            (concat (replace-regexp-in-string "::" "/" module-name) ".pm"))
           (perl-inc 
            (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_|||\"}'" ))
           (inc-path-list (split-string perl-inc "|||"))
           )
    (setq return
     (catch 'TANTRUM
       (dolist (inc-path inc-path-list)
         (setq full (concat (perlnow-fixdir inc-path) module-file-tail))
         (if (file-exists-p full)
             (throw 'TANTRUM full)))))
;    (message "Ret: %s" return) ; DEBUG only DELETE
    return))

   
;;;----------------------------------------------------------
(defun perlnow-module-two-questions (module-root module-name) 
  "Quickly jump into development of a new perl module"
  (interactive 
   ; Because default-directory is the default location for (interactive "D"),
   ; I'm doing the interactive call in two stages: change 
   ; default-directory momentarily, then restore it. Uses dynamic scoping via "let".
   ; (It's more like perl's "local" than perl's "my".)
   (let ((default-directory perlnow-module-root))
     (call-interactively 'perlnow-prompt-for-module-to-create)))
  (setq perlnow-perl-module-name module-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module module-root module-name)) )
    (perlnow-new-file-using-template filename perlnow-perl-module-template)))
   

;;;----------------------------------------------------------
(defun perlnow-prompt-for-module-to-create (where what) 
  "Ask the user two questions: the \"module root\" location, and the name 
of the perl module to create there.  Checks to see if one exists already, 
and if so, asks for another name.  The location defaults to the current 
`default-directory'.  Returns a two element list, location and module-name.\n
Note: This is used only by the mildly deprecated \[perlnow-module-two-questions\]."
  (interactive "DLocation for new module?  \nsName of new module \(e.g. New::Module\)? ")
  (let* ((filename (perlnow-full-path-to-module where what))
         (dirname (convert-standard-filename (file-name-directory filename))))
  (while (file-exists-p filename)
    (setq what 
          (read-from-minibuffer "That module name is already in use. Please choose another: " what))
    (setq filename (perlnow-full-path-to-module where what)))
  (list where what)))

;;;----------------------------------------------------------
(defun perlnow-set-run-string ()
  "Prompt the user for a new string that perlnow-run will
use to run the code in the current buffer. \n
From within a program, you'd probably be better off setting the variables 
directly, see `perlnow-script-run-string' and `perlnow-module-run-string'.\n

This function uses \\[perlnow-module-p] to see if the code looks like a
module (i.e. does it have a package line), otherwise it 
assumes it's a perl script."
;; And if it's not perl at all, that's your problem: the obvious
;; tests for perl code, like looking for the hash-bang,
;; aren't reliable (perl scripts need not have a hash-bang
;; line: e.g. *.t files, windows perl...).
  (interactive)
   (cond
   ((perlnow-module-p)
     ; set-up a decent default value
     (unless perlnow-module-run-string 
       (progn
         (setq perlnow-module-run-string 
               (perlnow-guess-module-run-string))))
     ; ask user how to run this module (use as default next time)
     (setq perlnow-module-run-string 
           (read-from-minibuffer 
            "Set the run string for this module: " 
            perlnow-module-run-string))
     ; tell perlnow-run how to do it
     (setq perlnow-run-string perlnow-module-run-string))
   (t  ;;  assume it's a script since it's not a module.
     ; set-up intelligent default run string 
     (unless perlnow-script-run-string 
       (progn
         (setq perlnow-script-run-string 
             (format "perl %s" (buffer-file-name)))))
     ; ask user how to run this script (use as default next time)
     (setq perlnow-script-run-string 
           (read-from-minibuffer 
            "Set the run string for this script: " 
            perlnow-script-run-string))
     ; tell perlnow-run to do it that way
     (setq perlnow-run-string perlnow-script-run-string))))

;;;----------------------------------------------------------
(defun perlnow-run (&optional runstring) ;;; is the optional okay there?
  "Run the perl code in this file buffer using `perlnow-run-string'
which typically will have been set by using \[perlnow-set-run-string].
If perlnow-run-string is nil, perlnow-set-run-string is called automatically."
  (interactive)
  (unless perlnow-run-string
    (perlnow-set-run-string))
;  (message "running with perlnow-run-string: %s" perlnow-run-string) ; debugging only  DELETE
  (compile perlnow-run-string))

;;;----------------------------------------------------------
(defun perlnow-h2xs (h2xs-location module-name) 
  "Quickly jump into development of a new perl module"
  (interactive 
; Because default-directory is the default location for (interactive "D"),
; I'm doing the interactive call in stages: this way can change 
; default-directory momentarily, then restore it. Uses the dynamic scoping 
; of elisp's "let" (which is more like perl's "local" than perl's "my").
  (let ((default-directory perlnow-module-root))
     (call-interactively 'perlnow-prompt-for-h2xs)))

  (let* ( (default-directory h2xs-location)
          (display-buffer (get-buffer-create "*perlnow-h2xs*")) )

  ;Bring the *perlnow-h2xs* display window to the fore (bottom window of the frame)
  (delete-other-windows) 
  (split-window-vertically -14) ; Number of lines of *.t to display
  (other-window 1)             
  (switch-to-buffer display-buffer) 

  (perlnow-blank-out-display-buffer display-buffer)
  (other-window 1)

   ; A typical h2xs run string:  h2xs -AX -n Net::Acme -b 5.6.0
  (call-process "h2xs"
                nil
                display-buffer      ; must be buffer object?
                nil
                "-AX"
                (concat "-n" module-name)
                (concat "-b" 
                        (perlnow-perlversion-old-to-new perlnow-minimum-perl-version)))

  (find-file 
   (perlnow-full-path-to-h2xs-module h2xs-location module-name))
  (search-forward "# Preloaded methods go here.")
  (forward-line 1)   ;alternate: (next-line 1)

  ; Also open the *.t file (note, this presumes the modern naming style)
  ; would break for older *.t style names, e.g. 1.t. TODO  Generalize?
  (other-window 1)
  (find-file
   (perlnow-full-path-to-h2xs-test-file h2xs-location module-name))
  (search-forward "BEGIN { plan tests => 1")
  (other-window 1)
  ))
;;; Note: my feeling is that asking two questions for the creation of an 
;;; h2xs structure is okay.  It helps differentiate it from perlnow-module, 
;;; and in any case it doesn't logically lend itself to a single question 
;;; form.  In the case of h2xs the "where" is the staging-area, 
;;; not the module-root... there are a couple of other levels between 
;;; the where and the what, and we might as well represent that gap as the 
;;; gap between the two questions.

;;;----------------------------------------------------------
(defun perlnow-prompt-for-h2xs (where what) 
  "Internal use only. Ask the user two questions: the
location to put the h2xs structure and the name of the perl
module to create.  Checks to see if one exists already, and
if so, asks for another name (by doing yet another
call-interactive of another function).  The location
defaults to the current default-directory. Returns a two
element list, location and module-name."
  (interactive "DLocation for new h2xs structure? \nsName of new module \(e.g. New::Module\)? ")

  (let ( location-in-staging-area        
         )
  (setq location-in-staging-area 
        (concat (perlnow-fixdir where)
                (mapconcat 'identity (split-string what "::") "-")))

  (while (file-exists-p location-in-staging-area)  ;;; really, directory exists
    (setq where-and-what  ; (h2xs-location module-name)
      (call-interactively 'perlnow-prompt-for-h2xs-again))
    (setq where (car where-and-what))
    (setq what (cadr where-and-what))

    (setq location-in-staging-area 
          (concat (perlnow-fixdir where)
                  (mapconcat 'identity (split-string what "::") "-")))
    )
    (list where what)))

;;;----------------------------------------------------------
(defun perlnow-prompt-for-h2xs-again (where what) 
  "Internal use only. If the user enters an existing h2xs
module name in \[perlnow-prompt-for-h2xs], it will do
another chained call-interactive to this function to ask
again with a slightly different message.  Returns a two
element list, location and module-name."
  (interactive "DThat exists already! Location for new h2xs structure? \nsName of new module \(e.g. New::Module\)? ")
  (list where what))


;;;==========================================================
;;; Internally used functions 
;;;==========================================================


;;;----------------------------------------------------------
(defun perlnow-full-path-to-module (module-root module-name)
  "Piece together a location and a perl-style module name into a full file name: 
given \"/home/doom/lib\" and \"Text::Gibberish\" would yield /home/doom/lib/Text/Gibberish.pm"
  (let ((filename 
         (concat 
          (mapconcat 'identity (split-string module-name "::") "/")
          ".pm")))
  (setq module-root (file-name-as-directory module-root)) 
  (concat  module-root filename)))

;;;----------------------------------------------------------
(defun perlnow-make-sure-file-exists()
  "Forcibly saves the current buffer to it's associated file, 
to make sure that the file actually exists."
  (set-buffer-modified-p t)
  (save-buffer))

;;;----------------------------------------------------------
(defun perlnow-change-mode-to-executable ()
  "Makes the file associated with the current buffer executable"
  (perlnow-make-sure-file-exists)
  (let* ((all-but-execute-mask ?\666)
         (filename (buffer-file-name))
         (file-permissions (file-modes filename))
         (new-file-permissions 
          (+ (logand file-permissions all-but-execute-mask) perlnow-executable-setting)
          ))
  (set-file-modes filename new-file-permissions)))


;;;----------------------------------------------------------
(defun perlnow-prompt-user-for-file-to-create (ask-mess default-location) 
  "Ask the user for the name of the script to create,
check to see if one exists already, and if so, ask for another name.  
Returns full file name with path."
  (let ( filename )
  (setq default-location (file-name-as-directory default-location)) 
  (while (progn 
           (setq filename 
                 (expand-file-name
                  (read-file-name ask-mess default-location)))
           (setq ask-mess 
                 "That name is already in use, please use another name: " )
           (file-exists-p filename)))
  (list filename)
  ))

  
;;;----------------------------------------------------------
(defun perlnow-new-file-using-template (filename template)
  "Given filename and template, does the actual creation of
the file and associated buffer using the template"
; Because of a bug in template.el, when using template-new-file 
; non-interactively, we must set the global "template-file" here:
  (setq template-file (template-split-filename filename)) 
  (template-new-file filename template)
  (write-file filename))

;;;----------------------------------------------------------
(defun perlnow-nix-script-p ()
  "Try to determine if the buffer looks like a 'nix style 
executable script by looking for the hash-bang line at the top."
  (save-excursion 
  (let ( (hash-bang-line-pat "^[ \t]*#!") )
    (goto-char (point-min))
    (looking-at hash-bang-line-pat) 
    )))

;;;----------------------------------------------------------
(defun perlnow-script-p ()
  "Try to determine if the buffer looks like a perl script by looking 
for the hash-bang line at the top.  Note: this is probably not a reliable
test, since some perl scripts will not have a hash-bang line, e.g. 
test files \(*.t\) or scripts on non-unix-like systems."
  (save-excursion 
  (let ( (hash-bang-line-pat "^[ \t]*#!.*perl\\b") ) ; note, presumes an explicit "perl"
    (goto-char (point-min))
    (looking-at hash-bang-line-pat))))

;;;----------------------------------------------------------
(defun perlnow-module-p ()
  "Determine if the buffer looks like a perl module by looking for the 
package line near the top."
  (save-excursion 
  (let ( (package-line-pat "^[ \t]*package\\b") 
         (comment-line-pat "^[ \t]*$\\|^[ \t]*#") )
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (looking-at package-line-pat) )))

;;;----------------------------------------------------------
(defun perlnow-get-module-name-from-module-buffer () 
  "Return the module name from the package line \(in perl's 
double colon separated form\), or nil if there is none"
  (save-excursion 
  (let ((package-line-pat "^[ \t]*package[ \t]*\\(.*\\)[ \t;]") ;; captures "Module::Name"
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
         return)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat) 
        (setq return (match-string 1))
      (setq return nil))
    return)))

;;;----------------------------------------------------------
(defun perlnow-get-module-name () 
  "Return the module name  \(in perl's double colon separated form\)
from either a module buffer or a Man page showing the perldoc for it, 
or nil if none is found"
;  (interactive) ; DEBUG only, DELETE
  (let (return)
    (cond 
     ((setq return (perlnow-get-module-name-from-module-buffer))
       )
     ((setq return (perlnow-get-module-name-from-man))
       )
     (t
      (setq return nil)
      ))
    (message "Returned module name=> %s" return)  ; DEBUG only, DELETE
    return))

;;;----------------------------------------------------------
(defun perlnow-get-module-name-from-man ()
  "Return the module name from a man page buffer displaying the perldoc.
If not a man page buffer, returns nil.  It tries several methods of 
scraping the module name from the man page buffer, and returns 
it's best guess."
;  (interactive)                         ; DEBUG only DELETE
  (save-excursion
    (let ( return buffer-name-string candidate-list 
           candidate-1 candidate-2 candidate-3 )
      (cond  
       ( (setq buffer-name-string (buffer-name))
         (if (string-match "\\*Man \\(.*\\)\\*$" (buffer-name))
             (progn
               (setq candidate-1 (match-string 1 buffer-name-string))
               (setq candidate-list (cons candidate-1 candidate-list))))

         (goto-char (point-min))
         (if (re-search-forward "NAME[ \t\n]*\\([^ \t]*\\)[ \t]" nil t)
             (progn
               (setq candidate-2 (match-string 1))
               (setq candidate-list (cons candidate-2 candidate-list))))

         (goto-char (point-min))
         (if (re-search-forward "SYNOPSIS[ \t\n]*use \\(.*\\)[ ;]" nil t)
             (progn
               (setq candidate-3 (match-string 1))
               (setq candidate-list (cons candidate-2 candidate-list))))

         (setq return 
               (perlnow-vote-on-candidates candidate-list))
;         (message "module name: %s" return) ; DEBUG only DELETE
         )
       (t 
        (setq return nil))))))

;;;----------------------------------------------------------
(defun perlnow-vote-on-candidates (candidate-list)
  "Pick most commonly occuring string from a list of strings"
  (let (score-alist)
    (dolist (candidate candidate-list)
      (let ((score 0))
        (dolist (compare candidate-list)
          (if (string= candidate compare)
              (setq score (+ 1 score)))
          )
        (setq score-alist (cons (cons candidate score) score-alist))))
    ; Now find max value in score-alist, return key.
    (let ( string score high_scorer
          (largest 0))
    (dolist (connie score-alist)
      (setq string (car connie))
      (setq score (cdr connie))
       (if (> score largest)
           (progn 
             (setq largest score)
             (setq high_scorer string))
             ))
    high_scorer)))

;;;----------------------------------------------------------
(defun perlnow-one-up (dir)
  "Gets an absolute path to the location one above the given location"
;;; TODO - Wouldn't string matches be simpler? 
;;;   (string-match "\\(^.*/\\)[^/]*$" (perlnow-fixdir dir))
;;;   (setq one-up (match-string 1 dir))

  (setq dir (perlnow-fixdir dir))
  (let ((return
         (concat "/" ; TODO - write func to prepend slash only if not already there?
                 (mapconcat 'identity 
                            (butlast 
                             (split-string dir "/") 
                             1) 
                            "/"))))
    (setq return (perlnow-fixdir return))
    return))

;;;----------------------------------------------------------
(defun perlnow-fixdir (dir)
  "Does the many cool and groovy elispy things that are a
good idea for conditioning directory paths for portability and 
robustness.  I don't always know when these things are needed, 
but now that I've got them all in this one, easy to use function, 
I will just use it all the goddamn time, and all of my problems 
will be a thing of the far distant extreme galactic past."
  (let ((return
  (convert-standard-filename
   (file-name-as-directory
    (expand-file-name dir)))))
    return))

;;;----------------------------------------------------------
(defun perlnow-expand-dots-relative-to (dot_means given_path)
  "Given a directory path that leads with  \".\" or \"..\" 
expand to an absolute path using the given dot_means as 
the value for \".\"."
;;; Note, this is limited to *leading* dot expressions, 
;;; Can't handle weirder stuff like: "/home/doom/tmp/../bin"
  (let ((two-dot-pat "^\\.\\.")  
        (one-dot-pat "^\\.")   ; must check two-dot-pat first or this could match there 
        newpath  )
   (setq dot_means (perlnow-fixdir dot_means))
   (setq newpath
         (replace-regexp-in-string two-dot-pat (perlnow-one-up dot_means) given_path))
   ; because perlnow-one-up uses perlnow-fixdir, no need to call it, (or to append "/" here)
   (setq newpath
         (replace-regexp-in-string one-dot-pat dot_means newpath))
   (setq newpath (perlnow-fixdir newpath))
   newpath))

;;;----------------------------------------------------------
(defun perlnow-lowest-level-directory-name (dir)
  "Return the lowest level directory name from a given path, 
e.g. Given: \"/usr/lib/perl/\" Returns: \"perl\" "
  (let* ( (levels (split-string dir "/"))
          (return (nth (- (length levels) 1) levels)) )
    return))

;;;----------------------------------------------------------
(defun perlnow-guess-module-run-string ()
  "Returns a good guess for an appropriate perlnow-module-run-string. 
First looks for the Makefile \(or Makefile.PL\) of an h2xs set-up.
Failing that it looks for a nearby test file of an appropriate name.
For example if the module were named New::Module, the test file 
could be New-Module.t or Module.t.  It searches the paths in 
`perlnow-test-path', which uses a familiar dot notation \(\".\" \"..\"\) 
to specify them relative to \"here\", where \"here\" means either 
the module-file-location or the module-root \(both interpretations 
are checked\). \n
If this seems too complex, that's because it is, but it does make 
it convenient to use a number of reasonable organizational schemes for 
your test files.\n
Note: Relying on the exact precedence of this search should be avoided
\(future versions may work slightly differently\)."
;;; Document those schemes for test file locations in detail.  Where? 
;;; A web page for now.
;;; TODO:
;;; o  Will also at some point want a "perlnow-edit-test-file-for-this-module".
;;; Maybe this code should be revamped (sigh): need code that returns test file name?
;;; o  Still another want would be "perlnow-create-test-file-for-module" which would need 
;;; to read policy from somewhere, to know where to put it and what to call it. 
;;; My pick for policy: if inside of an h2xs structure, put in the appropriate "t", 
;;; otherwise create a local t for it, but use the full hyphenized module name as 
;;; base-name (to make it easy to move around without confusion). 
;;; How to specify policy?  Three pieces of info: 
;;;   1 - A dot form, e.g. "./t"
;;;   2 - a definition of dot e.g. module-file-location
;;;   3 - name style, e.g. hyphenized

  (unless (perlnow-module-p) 
    (error "This buffer does not look like a perl module (no \"package\" line)."))
  (let* ( (module-name (perlnow-get-module-name-from-module-buffer))
          (module-file-location 
            (file-name-directory (buffer-file-name)))
          (module-root 
            (perlnow-get-module-root module-name module-file-location ))
          (hyphenized-module-name 
            (mapconcat 'identity (split-string module-name "::") "-"))
          (module-file-basename 
            (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

          ;;; TODO - Consider exposing a prototype of this list (somehow) to users,
          ;;;        via a defvar or something
          ; This is a listing of possible names for the test file:
          (test-file-check-list (list (concat hyphenized-module-name ".t")
                                      (concat module-file-basename ".t")
                                      )) ; note, h2xs test files need not be included in this list.

          staging-area      ; The location of an h2xs-style dev structure 
          staging-area-candidate staging-area-candidate-name 
          test-search-list  ; A listing of possible absolute locations to look for the test file, 
                            ; built up from relative locations in perlnow-test-path
          testloc testfile  
          fish water        ; going fishing
          return            ; the returned run string 
          ) 

    (setq return 
          (catch 'COLD
            (setq staging-area-candidate (perlnow-one-up module-root))
            (setq staging-area-candidate-name 
                  (perlnow-lowest-level-directory-name staging-area-candidate))
            (cond
             ((string= staging-area-candidate-name hyphenized-module-name)
              (setq staging-area staging-area-candidate) 
              (cond 
               ((file-regular-p (concat (perlnow-fixdir staging-area) "Makefile"))
                (setq water "make test")
                (throw 'COLD water))
               ((file-regular-p (concat (perlnow-fixdir staging-area) "Makefile.PL"))
                (setq water "perl Makefile.PL; make test")
                (throw 'COLD water)
                ))))

             ; do munging of dots, deal with different possible meanings of "here"
            (dolist (testloc-dotform perlnow-test-path) 
              (setq testloc 
                    (perlnow-expand-dots-relative-to module-file-location testloc-dotform))
              (if (file-directory-p testloc) 
                  (setq test-search-list (cons testloc test-search-list)))
              (setq testloc 
                    (perlnow-expand-dots-relative-to module-root testloc-dotform))
              (if (file-directory-p testloc) 
                  (setq test-search-list (cons testloc test-search-list))))

            ; tracking down the *.t file (if any)
            (dolist (real-place test-search-list) 
              (dolist (possible-name test-file-check-list)
                (setq testfile
                      (concat 
                       (perlnow-fixdir real-place) ;; I bet this fixdir is redundant
                       possible-name))
                (if (file-regular-p testfile)
                    (progn 
                      (setq fish (concat "perl " testfile))
                      (throw 'COLD fish)))))))
    return)) 

;;;----------------------------------------------------------
(defun perlnow-get-module-root (package-name module-location)
  "Given the package name and the module file location \(as an absolute path\),
determine the module-root, i.e. the place where the package namespace begins"
;; Example: 
;;  /home/doom/perldev/Punk/Skunk/New/Module.pm 
;;  /home/doom/perldev/Punk/Skunk/New/              => number of levels:  7
;;                                New::Module       => double-colon-count: 1
;;  /home/doom/perldev/Punk/Skunk/                  The desired module-root
;;
  (let (double-colon-count  ; count of '::' separators
        file-levels-list    ; list of directories in the path
        module-root)        ; 
    (setq double-colon-count (- (length (split-string package-name "::")) 1))
    (setq file-levels-list (split-string module-location "/"))
    (setq module-root (mapconcat 'identity 
                                 (butlast file-levels-list double-colon-count)
                                 "/"))
    (setq module-root (concat "/" module-root)) ; kludge, must prepend a "/" 
                                                ; (thus code breaks if not given full-path)
    module-root))


;;;----------------------------------------------------------
(defun perlnow-perlversion-old-to-new (oldver)
  "Converts old form of perl version \(e.g. 5.006\) into the new form 
\(i.e 5.6.0\), suitable for use as the -b parameter of h2xs"
;;;   (interactive "sinput old-syle perl version number, e.g. 5.006: ")  ; for debugging only
  (let ( (old-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)$")
         major
         mantissa 
         minor1)
;;;     (message "pattern: %s" old-version-pat) ; debug
  (if (string-match old-version-pat oldver)
      (progn 
        (setq major (match-string 1 oldver)) 
        (setq mantissa (match-string 2 oldver)))
    (error "Does not look like an old-style perl version: %s" oldver))
  (setq minor1 (substring mantissa 2))
  (concat major "." minor1 "." "0")))


;;;----------------------------------------------------------
(defun perlnow-full-path-to-h2xs-module (h2xs-location module-name)
  "Get the path to a module created by h2xs.  E.g. if the
h2xs-location were \"/usr/local/perldev\" and the module were
\"New::Module\", it should return: 
\"/usr/local/perldev/New-Module/lib/New/Module.pm\""
  (let ((module-filename 
         (concat 
          (file-name-as-directory h2xs-location)
          (mapconcat 'identity (split-string module-name "::") "-")
          "/lib/"
          (mapconcat 'identity (split-string module-name "::") "/")
          ".pm")))
;    (message "h2xs module file is %s"  module-filename) ; DELETE ?
    module-filename))

;;;----------------------------------------------------------
(defun perlnow-full-path-to-h2xs-test-file (h2xs-location module-name)
  "Get the path to a the test file for a module created by h2xs.  
E.g. if the h2xs-location were \"/usr/local/perldev\" and the 
module  were \"New::Module\", it should return: 
\"/usr/local/perldev/New-Module/t/New-Module.t\""
  (let* ( return 
         (module-test-location
          (concat 
           (file-name-as-directory h2xs-location)
           (mapconcat 'identity (split-string module-name "::") "-")
           "/t/"))
         (module-filename
          (concat 
           module-test-location
           (mapconcat 'identity (split-string module-name "::") "-")
           ".t")))
    (cond ((file-exists-p module-filename) 
           (setq return module-filename))
          ((file-directory-p module-test-location) 
           (setq return module-test-location))
           (t 
           (error "Can't find h2xs test file or test location")
           ))
;    (message "h2xs test file is %s"  module-filename)  ; DELETE ?
    module-filename))

;;;----------------------------------------------------------
(defun perlnow-blank-out-display-buffer (buffer)
  "Clear out a temporary display buffer, i.e. one whose 
name begins with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument can be a string or a buffer. 
This can work on a read-only buffer."

  (let ((original-buff (buffer-name))
        original-read-only-status)

  ; Buffer argument may be string or buffer object
  (if (char-or-string-p buffer) ; stringp better ? would a char work?
      (setq buffer (get-buffer-create buffer)))

  (if (not (string= "*" (substring (buffer-name buffer) 0 1)))
      (error "Will not blank out a buffer that does not begin with \"*\""))

  (if (buffer-live-p buffer) 
      (progn
        (set-buffer buffer)
        (setq original-read-only-status buffer-read-only)
        (setq buffer-read-only nil) ; make sure buffer is writeable
        (mark-whole-buffer)
        (delete-region (mark) (point))
        (setq buffer-read-only original-read-only-status) ; make it read-only if we found it that way
        )
    (get-buffer-create buffer)
  )))

;;;----------------------------------------------------------
(defun module-root-in-INC-p (&optional module-root)
  "Determine if the module-root \(i.e. the beginning of the package name space 
for a given module\) has been included in perls INC search path already \(and 
hence not in need of a \"use lib\"\). If not given a module-root, it 
defaults to using the module root of the current file buffer."
;;; Just checking getenv("PERL5LIB") would be close, but 
;;; using @INC as reported by perl seems more solid, so that's 
;;; what we do here.
;;; TODO That "|||" shit is cheesy.  Use "\t" or something, huh?
  (unless module-root
    (setq module-root 
          (perlnow-get-module-root 
           (perlnow-get-module-name-from-module-buffer)
           (file-name-directory (buffer-file-name)))))

    (let* (
      (perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_|||\"}'" ))
      (inc-path-list (split-string perl-inc "|||"))
      return )
      (setq return 
            (catch 'UP
              (dolist (path inc-path-list)
                (if (string= path module-root)
                    (throw 'UP t)))))
      return))
;;; TODO
;;; Consider loading a lisp structure with @INC once early on, 
;;; so we won't need to do the above over and over... 

;;;==========================================================
;;; perlnow-module now uses the following: 
;;;    perlnow-prompt-for-new-module-in-one-step
;;; to read in perlmodule path and names in one step
;;; (A variant of perlnow-prompt-for-module-to-create.)
;;;
;;; Note: instead of completing-read this uses read-from-minibuffer 
;;; directly, with a customized keymap to change it's behavior.
;;;==========================================================

;;;----------------------------------------------------------
(defvar perlnow-read-minibuffer-map
   (let ((map (make-sparse-keymap)))
     (define-key map "?"       'perlnow-read-minibuffer-completion-help)
     (define-key map " "       'perlnow-read-minibuffer-complete-word)
     (define-key map [tab]     'perlnow-read-minibuffer-complete)
     (define-key map "\C-g"    'abort-recursive-edit)
     (define-key map [return]  'exit-minibuffer)
     (define-key map [newline] 'exit-minibuffer)
     (define-key map [down]    'next-history-element)
     (define-key map [up]      'previous-history-element)
     (define-key map "\M-n"    'next-history-element)
     (define-key map "\M-p"    'previous-history-element)
     map)
   "Keymap for reading a perl module name via the minibuffer")
(put 'perlnow-read-minibuffer-map  'risky-local-variable t)
;;; TODO
;;; Look at minibuffer-local-map for hints on how to set up menu-bar: 
;;;     (define-key map [next] 'next-history-element)
;;;     (define-key map [prior] 'previous-history-element)


(defun perlnow-module (module-root module-name) 
  "Quickly jump into development of a new perl module.
In interactive use, gets path and module-name with a single 
question, asking for an answer in a hybrid form like so:
   /home/hacker/perldev/lib/New::Module
This uses the file-system separator  \"/\" for the module-root 
location and then the perl package name-space separator \"::\" 
for the module-name.  The \".pm\" extension is assumed 
and need not be entered. \n
If the module exists already, this will ask for another name. 
The location defaults to the global \[perlnow-module-root\], 
though this may be edited at run time."
;;; Formerly named: perlnow-prompt-for-new-module-in-one-step

;;; TODO more explication?

;;; The use must be careful to switch to double-colon separators 
;;; in the right place, to tell perlnow where the division is 
;;; in the namespaces. 

;Autocompletion works in a way very similar to the usual
;emacs input methods for file names and paths, but the
;transition to double-colon separators is used to indicate
;where perl's package namespace begins.  

;An example of typical input might be: \n
;   /usr/local/lib/perl/New::Module\n
;Where \"/usr/local/lib/perl/\" is the module-root and 
;\"New::Module\" is the module-name (aka package-name).\n

  (interactive 
   (let ((initial perlnow-module-root)
         (keymap perlnow-read-minibuffer-map)   ; Note: the keymap is key. Totally transforms read-from-minibuffer.
         (history 'perlnow-module-name-history) 
         result filename return
         )

     (setq result
           (read-from-minibuffer 
            "New module to create \(e.g. /tmp/dev/New::Mod\): " 
                                 initial keymap nil history nil nil))
     (setq filename (concat (replace-regexp-in-string "::" "/" result) ".pm"))

     (while (file-exists-p filename)
       (setq result
             (read-from-minibuffer 
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): " 
                                 result keymap nil history nil nil))
       (setq filename (concat (replace-regexp-in-string "::" "/" result) ".pm")))

     (setq return
           (perlnow-split-perlish-module-name-with-path-to-module-root-and-name result))
     return))

  (setq perlnow-perl-module-name module-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module module-root module-name)) )
    (perlnow-new-file-using-template filename perlnow-perl-module-template)))


;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-complete ()
  "Does automatic completion of up to an entire directory or file name 
if possible. Used in reading in path and name of a perl module \(which 
need not exist already\), where valid name separators are \(\"/\" or \"::\"\).\n
Makes no attempt at \"aggressive\" completion past a file-system 
boundary Not intended for non-interactive use."
;;; codename: new tabby
  (interactive)
  (let ((restrict-to-word-completion nil))
        (perlnow-read-minibuffer-workhorse restrict-to-word-completion)
    ))

;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-complete-word ()
  "codename: new spacey\n
Does automatic completion only up to the end of the next \"word\", 
as opposed to an entire directory or file name.
Used in reading in the name of a perl module name \(which need not 
exist already\), where valid name separators are \(\"/\" or \"::\"\).\n
Not intended for non-interactive use."
  (interactive)
  (let ((restrict-to-word-completion t))
    (perlnow-read-minibuffer-workhorse restrict-to-word-completion)
  ))

;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-workhorse (restrict-to-word-completion)
  "Does most of the actual work of auto-completion when reading 
reading in the name of a perl module name \(which need not 
exist already\), where valid name separators are \(\"/\" or \"::\"\).
Takes a single logical argument that controls whether whole name 
or single word completion will be used. "
;; codename: workhorse
;;;
;;; TODO 
;;; I'm hardcoding "/" as the "file system" separator 
;;; which means that this code will need to be fixed to get it 
;;; to work on a non unix-like system.
  (let* ( ; empty declarations:
         candidate-alist suggested-completion
         field-start 
         two-pieces-list perlish-path fragment fragment-pat file-system-path
         lastchar returned new-portion new-portion-first-word result new-mini
         word-separator
          ; assignments, setq's in all but name:
         (raw_string (buffer-string))
         (end-of-prompt-pat ": ")
         (pm-extension-pat "\\.pm$")
         )

         (string-match end-of-prompt-pat raw_string)
         (setq field-start (match-end 0)) ; also used later to blank minibuffer
         (setq minibuffer-string (substring raw_string field-start))

         ; No single trailing colons allowed: silently double them up
         (if (string-match "[^:]:$" minibuffer-string)
             (setq minibuffer-string (concat minibuffer-string ":")))

         ; Treat input string as a directory plus fragment
         (setq two-pieces-list
           (perlnow-split-module-path-to-dir-and-tail minibuffer-string))
         (setq perlish-path (car two-pieces-list))
         (setq fragment (cadr two-pieces-list))
         (setq fragment-pat (concat "^" fragment))                                                    

         (cond (; Are we inside the perl package namespace yet?
                (string-match "::" perlish-path) 
                (setq file-system-path (replace-regexp-in-string "::" "/" perlish-path))  
                   ; swap in file system separator "/"  for perl package separators "::" 
                (setq separator "::"))
               (t
                (setq separator "/")
                (setq file-system-path perlish-path)))

         (setq candidate-alist (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
         (setq returned (try-completion fragment candidate-alist))

         ; must convert logical values of "returned" into appropriate strings 
         (cond ((eq returned nil)  
                (setq suggested-completion fragment))
               ((eq returned t) ; precise match that is not a *.pm file is a directory, add separator
                 (if (string-match pm-extension-pat fragment)
                    (setq suggested-completion (substring fragment 0 (match-beginning 0) ))
                  (setq suggested-completion (concat fragment separator)))) 
               (t
                 (setq suggested-completion returned)))
        
         ; Prevents .pm extensions from appearing in the minibuffer
         (if (string-match pm-extension-pat suggested-completion) ; Yeah, checking *again*. Inelegant, but WTH
             (setq suggested-completion (substring suggested-completion 0 (match-beginning 0) )))

         ; if there's no change from the input value, go into help
         (setq result (concat perlish-path suggested-completion))
         (if (string= result minibuffer-string) 
             (perlnow-read-minibuffer-completion-help))

         ;;; peel off existing fragment from suggested-completion, what remains is the new-portion
         (string-match fragment-pat suggested-completion)
         (setq new-portion (substring suggested-completion (match-end 0)))
         (if restrict-to-word-completion  ; for "spacey" 
             (progn ; peel off word from the new-portion of suggested-completion
               (string-match "\\(^\\w*\\)\\(\\W\\|$\\)" new-portion)
               (setq new-portion-first-word
                     (match-string 1 new-portion))
               (setq word-separator ; save next non-word character: the "word-separator"
                     (match-string 2 new-portion))

                ;When new-portion-first-word is empty, we're at a word-separator
               (if (string= new-portion-first-word "")
                   (setq new-portion word-separator)
                 (setq new-portion new-portion-first-word))))

         (setq new-mini (concat perlish-path fragment new-portion))

         (delete-region (+ 1 field-start) (point-max))
         (insert new-mini)
         ))


;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-completion-help ()
   "The help command that displays a listing of available possible 
completions  when reading in the path and name to a perl module \(which 
need not exist already\), where valid name separators are 
\(\"/\" or \"::\"\).\n
Most likely this will be called by \\[perlnow-read-minibuffer-complete-word] 
and \\[perlnow-read-minibuffer-complete] \(at least indirectly, through 
\\[perlnow-read-minibuffer-workhorse])\), though it's also expected to 
be bound to the \"?\" key during the minibuffer read."
;;; codename: huh
  (interactive)
  (let* (
         (raw_string (buffer-string))
         (pat ": ")
         (field-start (+ (string-match pat raw_string) (length pat)))
         (string (substring raw_string field-start))
         ; Treat input string as a directory plus fragment
         (two-pieces-list
           (perlnow-split-module-path-to-dir-and-tail string))
         (perlish-path     (car two-pieces-list))
         (fragment (cadr two-pieces-list))
         (fragment-pat (concat "^" fragment)) ; for getting possible filename completions
                                              ; out of a list of bare filenames (no path)
         (file-system-path (replace-regexp-in-string "::" "/" perlish-path) )  
            ; unix file system separator "/" swapped in for perl package separators "::" 
         (match-alist (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
;;;         (file-list (mapcar '(lambda(pair) (car pair)) match-alist))
           ;;; could try file-list in place of the all-completions call. (yields stringp: NG?)
        )
   (setq match-alist (perlnow-remove-pm-extensions-from-alist match-alist))
   (with-output-to-temp-buffer "*Completions*"
     (display-completion-list
      (all-completions fragment match-alist)
      ))
   ))

;;;----------------------------------------------------------
(defun perlnow-remove-pm-extensions-from-alist (alist)
  "Go through an alist of file names and values, removing the 
pm extension from the end of any file names in which it appears."
;Note that this actually throws away the numeric value and generates 
;a new one: I don't expect that this matters."
  (let (name new-alist (i (length alist)) )
    (dolist (pair alist)
      (setq name (car pair))
      (setq name (replace-regexp-in-string "\\.pm$" "" name))
      (setq new-alist (cons (cons name i) new-alist))
      (setq i (- i 1))
      )
   (setq new-alist (reverse new-alist))
   ))

;;;----------------------------------------------------------
(defun perlnow-list-directories-and-modules-as-alist (file-system-path pattern)
  "Gets a directory listing from the given path, and returns
an alist of the file and directory names that match certain criteria. 
All the names must match the given pattern \(expected
to be of the form \"^leading_fragment\"\).  Further, the filenames 
are restricted to being perl module names \(ending in \"*.pm\"\) 
which also pass the \[perlnow-interesting-file-name-p] test. \n 
These are simple file names that do not include the path, 
and the values associated with them in the returned alist 
are sequential integers."
;;; For extra credit how about stripping the .pm on the file names?
;;; Nope: I can't do that, it messes up "workhorse" as written. 
   (let* ( 
          match-alist
          ; directory-files directory &optional full-name match-regexp nosort
          (directory-full-name nil)
          (directory-nosort nil)
          (file-list 
            (directory-files file-system-path directory-full-name pattern directory-nosort))
;          base-name
          (i 1)  ; counter to build alist with numeric value
          )
     (dolist (file file-list)
       (if (perlnow-interesting-file-name-p file)
           (cond ((file-directory-p (concat file-system-path file))
                   (setq match-alist (cons (cons file i) match-alist))
                   (setq i (+ i 1)))
                 ((string-match "\\.pm$" file)
;                 (string-match "^\\(.*\\)\\.pm$" file)
;                   (setq base-name (match-string 1 file))
;                   (setq match-alist (cons (cons base-name i) match-alist))
                   (setq match-alist (cons (cons file i) match-alist))
                   (setq i (+ i 1))))))
  ; Reverse the order of the match-alist to get values counting up starting from 1
  (setq match-alist (reverse match-alist))  ;; maybe this isn't needed, but cargo cult programming is fun
  ))


;;;----------------------------------------------------------
(defun perlnow-list-directory-as-alist (file-system-path pattern)
  "Gets a directory listing from the given path, and returns
an alist of the file names that match the given pattern, *and*
which also pass the \[perlnow-interesting-file-name-p]
test. These are simple file names not including the path, and
the values associated with them in the alist are sequential numbers"
;;; Functional, but most likely NOT USED 
   (let* ( 
          match-alist
          ; directory-files directory &optional full-name match-regexp nosort
          (directory-full-name nil)
          (directory-nosort nil)
          (file-list 
            (directory-files file-system-path directory-full-name pattern directory-nosort))
          (i 1)  ; counter to build alist with numeric value
          )
     (dolist (file file-list)
       (if (perlnow-interesting-file-name-p file)
           (progn
             (setq match-alist (cons (cons file i) match-alist)) 
             (setq i (+ i 1))
         )))
  ; Reverse the order of the match-alist
  (setq match-alist (reverse match-alist))  ;; maybe this isn't needed, but cargo cult programming is fun
  ))


;;;----------------------------------------------------------
(defun perlnow-split-perlish-module-name-with-path-to-module-root-and-name (string)
  "Input string is expected to be a hybrid file system
path using slashes for the module root name space, and
double colons for the package name space inside of that. \n
This input is split into two pieces, the module-root 
and module-name, which are returned in a list."
  (let* ( (pattern 
            (concat 
             "^\\(.*\\)"       ; ^(.*)    - stuff at start becomes the mod root
             "/"               ; /        - the right-most slash, because: 
             "\\([^/]*\\)"     ; ([^/]*)  - mod name: everything that is not a slash up to  --
             "\\(\\.pm\\)*$"   ; (\.pm)*$ - the end (or an optional .pm extension)
             ))
           module-root 
           module-name
          )
         (cond ((string-match pattern string)
                (setq module-root (match-string 1 string))
                (setq module-name (match-string 2 string)) ) ; note: does not include any .pm
               (t
                (message "match failed: could not separate into module root and name.") )) 
         (list module-root module-name) ))


;;;----------------------------------------------------------
(defun perlnow-interesting-file-name-p (string)
  "Takes a bare filename (sans path) in the form of a
string, returns t if it doesn't match the list of
uninteresting filenames patterns, otherwise nil."
;; TODO DONTFORGET
;; Don't just silently use completion-ignored-extensions or indeed 
;; anything hardcoded in this function. Break out as a defvar  
;; "perlnow-interesting-file-name-pat" or something.
;; Let the user define what's interesting. 
  (let ( 
         (ignore-pat  
           (concat "\\("     
                   (mapconcat 'regexp-quote completion-ignored-extensions "\\|")
                   "\\)$"
                   "\\|"   ; also skip the dot files "." and ".."
                   "^\\.$"
                   "\\|"
                   "^\\.\\.$"
                   ))
         )
    (unless (stringp string)
      (error "Expected string in input"))
    (not (string-match ignore-pat string))
    ))

;;;----------------------------------------------------------
(defun perlnow-split-module-path-to-dir-and-tail (string)
  "Splits a file system path into directory and trailing 
fragment, allowing for the use of perl's double-colon 
package name separators in addition to the usual unix-like 
slash character.  \n
Simple example: given \"/home/doom/lib/Stri\" should return 
 \"/home/doom/lib/\" and \"Stri\"\n
Perl package example: given \"/home/doom/lib/Taxed::Reb\" should return 
 \"/home/doom/lib/Taxed::\" and \"Reb\"\n"
  (let* ( (pattern "^\\(.*\\(/\\|::\\)\\)\\([^/:]*$\\)" )
           directory fragment
          )
         (cond ((string-match pattern string)
                (setq directory (match-string 1 string))
                (setq fragment (match-string 3 string)) )
               (t
                (message "match failed") )) 
         (list directory fragment) ))

;;;==========================================================
;;; Older code 
;;;==========================================================

;;; TODO  DONTFORGET
;;; Maybe: include the old perlutil-* routines. 
;;; Detect if template.el is installed, and if not, 
;;; fall back on using these (instant gratification principle... 
;;; try and do something useful, even if the installation isn't 
;;; quite right... but notify somehow that there's a problem to 
;;; be fixed). 

;;;----------------------------------------------------------
;;;   (defun perlutil-perlnow ()
;;;----------------------------------------------------------
;;;   (defun perlutil-perlify-this-buffer ()






;;;==========================================================
;;; Experimental code (possibly unfinished)
;;;==========================================================

;;;----------------------------------------------------------
;; (defun quote-regexp-stupid-backwhacks (string) 
;;   "Takes an ordinary regexp string like (\tset|\techo) and 
;; turns into into a string like \"\\(\tset\\|\techo\\)\", which is 
;; suitable for tranformation into an emacs regexp, namely 
;; \"\(\tset\|\techo\)\".  Note this leaves an escape like 
;;  \"\t\" alone (it becomes a literal tab internally)."
;; ;;; TODO  NOT FINISHED
;; ;;; There are other things like \w and \W that also need the 
;; ;;; double escape.
;;   (let ((specious-char-class "[(|)]"))
;;         ) 
;;   ;;; want to match for all values of specious-char-class, 
;;   ;;; capturing and replacing with "\\c" where "c" is whatever 
;;   ;;; value was just matched.  
;; ))


;;;===========================================================================
;;;  History 
;;;===========================================================================

;; This began life as excerpts from "perlutil.el" -- which was not distributed.
;; It was time to clean-up some cruft now that I was settling on  
;; using template.el templating, and I wanted to do a package re-name in any case 
;; ("perlutil" already means something else in the perl world, "man perlutil" 
;; will tell you about utilities that come with perl).

;; The main concept is to make it easy to quickly jump into perl development.
;; It's first incarnation was a keyboard macro that insert the hashbang line. 
;; Then came some elisp that could do useful things like make the new file 
;; executable, and insert a standard header with title, email and date. 
;; Then I wanted to get away from the hard-coded header, so I started 
;; experimenting with templating systems -- following the golden (silver? 
;; tin?) rule: "though shalt not invent yet another templating 
;; system" I started looking at the various existing lisp packages.  
;; The tempo.el package had some promise, but also some problems and it's largely
;; designed for other things (heavily interactive uses, like writing new major 
;; modes; and it presumes template authors are familiar with lisp).  
;; I've settled on template.el as being closest to what I want 
;; though it's also probably more complicated than what I need, probably 
;; distributed and I've had to kludge a work-around in an existing bug 
;; isn't widely (which I will report someday if I can understand it well
;; enough... like I said, excessively complicated). 

;; See here for more:
;;   /home/doom/End/Hack/Emacs/notes-perlutil
;;   /home/doom/End/Hack/Emacs/notes-perlnow

;;; perlnow.el ends here
