;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.9 2004/01/31 06:59:55 doom Exp root $
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

; The main idea is to automate the routine tasks when you
; begin work on a new file of perl code.  A single perlnow
; command will typically prompt for a location and a name,
; open a file buffer with an appropriate framework already
; inserted (e.g. the hash-bang line, comments including date
; and author information, a perldoc outline, and so on). In
; the case of scripts the file automatically become executable.

; To function properly, it requires that template.el has been 
; installed, along with two templates for perl development 
; purposes, one for scripts, another for modules.  Most likely
; ~/.templates is the place these templates should be installed.

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

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlnow)
;;   (global-set-key  "\M-ps" 'perlnow-script)
;;   (global-set-key  "\M-pm" 'perlnow-module)
;;   (setq perlnow-script-location (substitute-in-file-name "$HOME/bin"))
;;   (setq perlnow-module-root (substitute-in-file-name "$HOME/lib"))
;; 

;;; Definitions of some terms used: 
;;; TODO - expand this
;;; TODO - check perl definitions, e.g. precise distinction between package and module.
;;; module-file-name - the file system's name for the module file, e.g. /usr/lib/perl/Double/Colon.pm
;;; module-file-basename - name of the module file itself, sans extension: in the above example, "Colon"
;;; module-location  - directory portion of module-file-name, e.g. /usr/lib/perl/Double/
;;; module-name or package-name - perl's double colon separated name, e.g. "Double::Colon"
;;; module-root - The place where perl's double-colon name space begins (e.g. /usr/lib/perl)
;;;               The PERL5LIB environment variable is a list of different module-roots. 
;;; staging-area - the directory created by the h2xs command for module development, 
;;;                a "hyphenized" form of the module-name e.g. Double-Colon.
;;;                Every staging-area contains a module-root called "lib".
;;; h2xs-location - the place where you put your staging-areas (possibly /home/doom/dev ?)

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

; I'm grabbing "Alt p" as the perlnow prefix, but using the
; cperl-mode-map and perl-mode-map since perlnow doesn't
; have a map of it's own since it's not a mode.  

; Is Alt-p okay? (It *and* Alt-n are undefined by default, 
; but that suggests some intent to use them as customizeable 
; navigation bindings)   ((Yeah, navigates history in a sub shell!))

; Also need global bindings for "perlnow-script" and 
; "perlnow-module": want them to jump in to perl programming 
; fast.  Probably can't rightly do global settings by default 
; though. 

(add-hook 'cperl-mode-hook
          '(lambda ()
             (define-key cperl-mode-map "\M-pb" 'perlutil-perlify-this-buffer)
             (define-key cperl-mode-map "\M-pn" 'perlutil-perlnow)
             (define-key cperl-mode-map "\M-pc" 'perlnow-run-check)
             ))

(add-hook 'perl-mode-hook
          '(lambda ()
             (define-key cperl-mode-map "\M-pb" 'perlutil-perlify-this-buffer)
             (define-key cperl-mode-map "\M-pn" 'perlutil-perlnow)
             (define-key cperl-mode-map "\M-pc" 'perlnow-run-check)
             ))

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
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perl.tpl")
"The template.el template new perl scripts will be created with" )

(defvar perlnow-perl-module-template 
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.pm.tpl")
"The template.el template new perl modules will be created with" )

(defvar perlnow-perl-module-name nil
  "Used internally to pass the a module name in the perl
double-colon separated form to the template.el module
template ")
;;; TODO - Look for a way to do this without a global variable

;;;----------------------------------------------------------
;;; Add feature PERL_MODULE_NAME for the perlnow-module function 
;;; (an "expansion" used in a template.el template like so: 
;;; (>>>PERL_MODULE_NAME<<<);
(setq template-expansion-alist 
      (cons 
      '("PERL_MODULE_NAME" (insert perlnow-perl-module-name) )
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

;;; Developer note: eval this to erase effects of the above two settings:
;;; (setq template-expansion-alist 'nil)


;;;==========================================================
;;; User Commands
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-run-check ()
  "Run a perl check on the current buffer, displaying errors
and warnings in another window.  Afterwards, you can skip to
the location of the next problem with \\[next-error] \n This
command is like \\[cperl-check-syntax] with one less prompt
(also, it does not require mode-compile.el)"
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
in the currently open buffer"
  (interactive 
  (perlnow-prompt-user-for-file-to-create 
   "Name for the new perl script? " perlnow-script-location))
  (let (package-name
        (module-filename (buffer-file-name))) 
;;; read the package name out of the current module file buffer:
  (save-excursion 
    (let ((comment-line-pat "^[ \t]*$\\|^[ \t]*#") 
          (package-line-pat "^[ \t]*package \\(.*\\)[ \t]*;"))
      (goto-char (point-min))
      (while (looking-at comment-line-pat) (forward-line 1))
      (if (looking-at package-line-pat)
          (setq package-name (match-string 1))
        (error "%s" "No leading package line: This file doesn't look like a perl module"))))
        ;;; create new script file buffer using template
  (perlnow-new-file-using-template filename perlnow-perl-script-template)
;;; insert the lib path tweaks to ensure the module can be found by the script
  (let ((relative-path
         (file-relative-name module-filename (file-name-directory filename))
         ))
    (insert "use FindBin qw\($Bin\);\n")
    (insert "use lib \(\"$Bin/")
    (insert relative-path)
    (insert "\");\n"))
        ;;; insert the "use Some::Module;" line
  (insert (format "use %s;" package-name)) ;;; and maybe a qw() list? 
  (insert "\n")))

;;; TODO: Question: is there something intelligent that could be done 
;;; with EXPORT_OK to provide a menu of options here? 
;;; Should you qw() *all* of them (maybe except for :all) and let user delete?

;;; TODO
;;; Maybe someday: check first PERL5LIB and don't add FindBin jazz if module 
;;; is already findable.  Easy enough: (getenv "PERL5LIB") Search through result 
;;; for matching module-root 
;;; Duh, this ain't good enough: (file-name-directory module-name).

;;; TODO: Option to insert the SYNOPSIS section in commented out form

;;;==========================================================
;;; functions to allow creation of new *modules* not scripts

;;; TODO:  would like to accept slash separated form of the name
;;;        as well as double colon notation.
;;;       (Appending *.pm should be allowed, but not required)
;;;       Autocompletion on the part of the module name that corresponds to 
;;;       a directory would be cool... 

;;; Note: before you look into this stuff too much, see what h2xs does 
;;; *for* you.


;;;----------------------------------------------------------
(defun perlnow-module (module-root module-name) 
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
and if so, asks for another name ((TODO-check that)).  The location defaults to the current 
`default-directory'.  Returns a two element list, location and module-name."
  (interactive "DLocation for new module?  \nsName of new module \(e.g. New::Module\)? ")
  (let* ((filename (perlnow-full-path-to-module where what))
         (dirname (convert-standard-filename (file-name-directory filename))))
  (while (file-exists-p filename)
    (setq what 
          (read-from-minibuffer "That module name is already in use. Please choose another: " what))
    (setq filename (perlnow-full-path-to-module where what)))
  (list where what)))


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


;;;==========================================================
;;; Older code 
;;;==========================================================

;;; TODO 
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
;;; Internally used functions 
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-make-sure-file-exists()
  "Forcibly saves the current buffer to it's associated file, 
to make sure that the file actually exists."
; inserting and deleting a space to make sure it's considered "modified" 
; and in need of saving. 
; TODO (TEST): should probably just do this:
  (set-buffer-modified-p t)
;  (insert " ") 
;  (delete-backward-char 1) 
  (save-buffer))

;;;----------------------------------------------------------
(defun perlnow-change-mode-to-executable ()
  "Makes the file associated with the current buffer executable"
; Need to make sure the file really exists before we chmod it
; so we save it, but to make sure that happens it has to be "modified":
  (perlnow-make-sure-file-exists)
  (let* ((all-but-execute-mask ?\666)
         (filename (buffer-file-name))
         (file-permissions (file-modes filename))
         (new-file-permissions 
          (+ (logand file-permissions all-but-execute-mask) perlnow-executable-setting)
          ))
;  (setq file-permissions (file-modes filename))
;  (setq new-file-permissions 
;    (+ (logand file-permissions all-but-execute-mask) perlnow-executable-setting))
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

;;; Because of a bug in template.el, when using template-new-file 
;;; non-interactively, we must set the global "template-file" here:
  (setq template-file (template-split-filename filename)) 
  (template-new-file filename template)
  (write-file filename))


;;;----------------------------------------------------------
(defun perlnow-script-p ()
  "Try to determine if the buffer looks like a perl script by looking 
for the hash-bang line at the top.  Note: this is probably not a reliable
test, since some perl scripts will not have a hash-bang line, e.g. 
test files \(*.t\) or scripts on non-unix-like systems."
  (save-excursion 
  (let (
        (hash-bang-line-pat "^[ \t]*#!.*perl\\b") ; note, presumes an explicit "perl"
        pee)
    (goto-char (point-min))
    (if (looking-at hash-bang-line-pat) 
        (setq pee 't)
      (setq pee 'nil))
    pee)))


;;;----------------------------------------------------------
(defun perlnow-module-p ()
  "Determine if the buffer looks like a perl module by looking for the 
package line near the top."
  (save-excursion 
  (let ((package-line-pat "^[ \t]*package\\b") 
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
        pee)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat) 
        (setq pee 't)
      (setq pee 'nil))
    pee)))

;;;----------------------------------------------------------
(defun perlnow-get-module-name () 
  "Return the module name from the package line, or nil if there is none"
;  (interactive) ; DEBUG only, DELETE
  (save-excursion 
  (let ((package-line-pat "^[ \t]*package[ \t]*\\(.*\\)[ \t;]") ;; captures "Module::Name"
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
         return)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat) 
        (setq return (match-string 1))
      (setq return nil))
    return
;   (message "Ret: %s" return)  ; DEBUG only, DELETE
   )))


;;;----------------------------------------------------------
;;; I am following my instinct and using make-variable-buffer-local to
;;; force the following to always be buffer-local, despite the
;;; admonition in the manual (which has a "religious issue" smell to
;;; it).  My reasoning is that this makes the following code simpler (I
;;; don't want to have to remember to use make-local-variable in
;;; different places), and I can't think of a case where the user 
;;; would be annoyed at me depriving them of this choice. 

(defvar perlnow-script-run-string nil 
"The run string for perl scripts, used by \[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(make-variable-buffer-local 'perlnow-script-run-string)

(defvar perlnow-module-run-string nil 
"The run string for perl modules, used by \[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(make-variable-buffer-local 'perlnow-module-run-string)

(defvar perlnow-run-string nil 
"A buffer local variable, set by \[perlnow-script-run-string] to tell 
\[perlnow-run] how to run the code in a particular file buffer.  This should 
not typically be set by the user directly.  See `perlnow-script-run-string' 
and `perlnow-module-run-string' instead.")
(make-variable-buffer-local 'perlnow-run-string)


;;;----------------------------------------------------------
;;;----------------------------------------------------------
(defun perlnow-set-run-string ()
  "Prompt the user for a new string that perlnow-run will
use to run the code in the current buffer.  
Don't use this from within a program: instead set the variables 
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
               (perlnow-choose-module-run-string))))
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
  (message "running with perlnow-run-string: %s" perlnow-run-string) ; debugging only  DELETE
  (compile perlnow-run-string))

;;;----------------------------------------------------------
(defun perlnow-one-up (dir)
  "Gets an absolute path to the location one above the given location"
;  (interactive "Dgimme a dir: ") ; DEBUG only DELETE
  (setq dir (perlnow-fixdir dir))
  (let ((return
         (concat "/" ; TODO - might be good to have a func to prepend slash only if not already there 
                 (mapconcat 'identity 
                            (butlast 
                             (split-string dir "/") 
                             1) 
                            "/"))))
    (setq return (perlnow-fixdir return))
;    (message "ret: %s" return) ; DEBUG only DELETE
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
;  (interactive "sGivenpath: \nsDot expansion: "); DEBUG only DELETE
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
;   (message "newpath: %s" newpath) ; DEBUG only DELETE
   newpath))

;;;----------------------------------------------------------
(defun perlnow-lowest-level-directory-name (dir)
  "Return the lowest level directory name from a given path, 
e.g. Given: \"/usr/lib/perl/\" Returns: \"perl\" "
;;;  (interactive "Dgimme a place: ") ; DEBUG only DELETE
  (let* ( (levels (split-string dir "/"))
          (return (nth (- (length levels) 1) levels)) )
    return))


(defvar perlnow-test-path (list "." "../t" "./t")
"List of places to look for test scripts (*.t), typically these will be relative paths.")

;;;----------------------------------------------------------
(defun perlnow-choose-module-run-string ()
  "Returns a good guess for an appropriate perlnow-module-run-string. 
First looks for the Makefile \(or Makefile.PL\) of an h2xs set-up.
Failing that it looks for a nearby test file which would be named
New-Module.t or Module.t, if the module were named New::Module.
It searches the paths in perlnow-test-path, which are relative 
to \"here\", where \"here\" means either the module-file-location or 
the module-root.  This allows for a number of reasonable 
organizational schemes for test files; though relying on the current
precedence of this search should be avoided \(future versions 
may work slightly differently\)."
;;; Document in more detail?  Maybe elsewhere? 
;;; TODO:
;;; o  Will also at some point want a "perlnow-jump-into-test-file-for-module".
;;; Maybe this code should be revamped, want code that returns test file name?
;;; o  Still another: would be perlnow-create-test-file-for-module which would need 
;;; to read policy from somewhere, to know where to put it and what to call it. 
  (interactive) ;;; DEBUG only DELETE
  (unless (perlnow-module-p) 
    (error "This buffer does not look like a perl module (no \"package\" line)."))
  (let* ( (module-name (perlnow-get-module-name))
          (module-file-location 
           (file-name-directory (buffer-file-name)))
          (module-root 
           (perlnow-get-module-root module-name module-file-location ))
          (hyphenized-module-name 
           (mapconcat 'identity (split-string module-name "::") "-"))
          (module-file-basename 
           (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
          (test-file-check-list '( (concat (hyphenized-module-name) ".t")
                                   (concat (module-file-basename) ".t")
                                  ; ("1.t") ;;; current thought: just let h2xs Makefile handle this
                                   ))
          staging-area
          maybe-staging-area
          maybe-staging-area-name
          testloc
          test-search-list       
          testfile
          water                  ; use when going fishing
          fish                   ; similar 
          result                 ; the returned run string 
          ) 

    (setq result 
          (catch 'COLD
            (setq maybe-staging-area (perlnow-one-up module-root))
            (setq maybe-staging-area-name (perlnow-lowest-level-directory-name maybe-staging-area))
            (cond
             ((string= maybe-staging-area-name hyphenized-module-name)
              (setq staging-area maybe-staging-area) 
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
                (if (file-regular-p fish)
                    (progn 
                      (setq fish (concat "perl " testfile))
                      (throw 'COLD fish)))))))
    result)) 



;;;----------------------------------------------------------
(defun perlnow-get-module-root (package-name module-location)
  "Given the module file location and the package name, determine module-root"
;  (interactive "sPackage Name: \nsModule Location: ") ;;; DEBUG only DELETE
;;; WORKS well enough.  TODO Cleanup names (function and variables)
  (let (double-colon-count file-levels module-root)
    (setq double-colon-count (- (length (split-string package-name "::")) 1))
    (setq file-levels (split-string module-location "/"))
    (setq module-root (mapconcat 'identity 
                                 (butlast file-levels double-colon-count)
                                 "/"))
    (setq module-root (concat "/" module-root)) ; kludge, prepends a "/" 
                                                ; (code will break if not given full-path)
;   (message "Module root: %s" module-root)
    module-root))

  ;;; Example: 
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/New/Module.pm 
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/New/          => number of levels:  7
  ;;;                                    New::Module   => double-colon-count: 1
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/                 The desired module-root




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


;;; h2xs -AX -n Net::Acme -b 5.6.0

;;;----------------------------------------------------------
(defun perlnow-h2xs (h2xs-location module-name) 
  "Quickly jump into development of a new perl module"
  (interactive 
; Because default-directory is the default location for (interactive "D"),
; I'm doing the interactive call in two stages: this way can change 
; default-directory momentarily, then restore it. Uses dynamic scoping via "let".
; (which is more like perl's "local" than perl's "my".)
  (let ((default-directory perlnow-module-root))
    (call-interactively 'perlnow-prompt-for-h2xs)))

  (let* ( (default-directory h2xs-location)
          (display-buffer (get-buffer-create "*perlnow-h2xs*")) )

  ;Bring the *perlnow-h2xs* display window to the fore (bottom window of the frame)
  (delete-other-windows) 
  (split-window-vertically -12)       ; Expected size of output from h2xs is 6 lines
  (other-window 1)                    ; with some re-centering, could trim this "-12".
  (switch-to-buffer display-buffer) 

  (perlnow-blank-out-display-buffer display-buffer)
  (other-window 1)

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
 ; (delete-other-windows) 
  ))

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
    (message "h2xs module file is %s"  module-filename)
    module-filename))

;;;----------------------------------------------------------
(defun perlnow-blank-out-display-buffer (buffer)
  "Clear out a temporary display buffer, i.e. one whose 
name begins with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument can be a string or a buffer. 
This can work on a read-only buffer."
;;; TODO - this looks like it should be rewritten to use cond
; DELETE
; just for debuggery:
;   (interactive "Bgimme a buffy: ")                     
;   (interactive "stype in a buffer name carefully: ") 
; END DELETIA

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



;;;==========================================================
;;; Experimental code can go below here     BOOKMARK (note: trying reg-*)

   
;;;----------------------------------------------------------
(defun perlnow-prompt-for-h2xs (where what) 
  "Ask the user two questions: the location to put the h2xs structure 
and the name of the perl module to create.  Checks to see if one exists already, 
and if so, asks for another name.  The location defaults to the current 
default-directory. Returns a two element list, location and module-name."

  (interactive "DLocation for new h2xs structure? \nsName of new module \(e.g. New::Module\)? ")
  (list where what))
;;; TODO - Still need to check for already existing module. 

;;; TODO - would like to allow more varied types of input. 
;;;        Why not:  /home/perl/modev/New::Module ?
;;;        or even:  /home/perl/modev/New::Module.pm
;;;  Unfortunately that this can't be made to work: 
;;;        /home/perl/modev/New/Module.pm
;;;  (no way to pick out the module-root reliably).

;;; DELETE   
;;; check for existance of what?  The directory?  Then you need to convert the module 
;;; name into the directory name (I think '::' -> '-'), and append that to where. 

;;; call this routine again (recursively) if we need another one? 
;;; cute, but hard to modify the prompt that way... unless it's 
;;; a *string*, defined with "let" in the calling routine above.  Hm.

;  (let* ((filename (perlnow-full-path-to-module where what))
;         (dirname (convert-standard-filename (file-name-directory filename))))
                        ;    (while (file-exists-p filename)
                                        ;       (setq what 
;          (read-from-minibuffer "That module name is already in use. Please choose another: " what))
;       (setq filename (perlnow-full-path-to-module where what)))
;;; END DELETIA







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
