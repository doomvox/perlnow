;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.7 2004/01/28 19:54:46 doom Exp root $
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
; the case of scripts the file will automatically become
; executable.

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
; exist and create them otherwise. 
; Or see if they exist, and then use them, if not, silently fall back on HOME?

(defvar perlnow-script-location (getenv "HOME")
  "The default location to stash new perl scripts")
(setq perlnow-script-location (file-name-as-directory perlnow-script-location)) 

(defvar perlnow-module-root (getenv "HOME")
  "The default location to stash new perl modules")
(setq perlnow-module-root (file-name-as-directory perlnow-module-root))

(defvar perlnow-executable-setting ?\110
  "Pattern of user-group-all permission settings used when making a script executable")

(defvar perlnow-perl-script-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.perl.tpl")
"The template.el template new perl scripts will be created with" )
;;; DELETE:
;;; (setq perlnow-perl-script-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.perl.tpl"))

(defvar perlnow-perl-module-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.pm.tpl")
"The template.el template new perl modules will be created with" )
;;; DELETE:
;;; (setq perlnow-perl-module-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.pm.tpl"))

(defvar perlnow-perl-module-name 'nil
  "Used internally to pass the a module name in the perl
double-colon separated form to the template.el module
template ")
;;; TODO - Look for a way to do this without a global variable

;;; template.el feature PERL_MODULE_NAME for the perlnow-module function. 
(setq template-expansion-alist 
      (cons 
      '("PERL_MODULE_NAME" (insert perlnow-perl-module-name) )
      template-expansion-alist))

(defvar perlnow-minimum-perl-version "5.006"
  "Used to replace template MINIMUM_PERL_VERSON with the
minimum perl version you're planning on supporting. Note
that perl version numbers jumped from 5.006 to 5.7.0.  As of
this writing, the latest is 5.8.2 ((CHECK)) ")
; Defining template.el feature MINIMUM_PERL_VERSON to be used like: 
                        ;   use (>>>MINIMUM_PERL_VERSON<<<);
(setq template-expansion-alist 
      (cons 
      '("MINIMUM_PERL_VERSON" (insert perlnow-minimum-perl-version))
      template-expansion-alist))

;;; DELETE ME
;;; Eval this to erase effects of the above two settings:
;;; (setq template-expansion-alist 'nil)
;;; END DELETIA

;;;==========================================================
;;; User Commands
;;;==========================================================

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


(defun perlnow-script (filename)
  "Quickly jump into development of a new perl script"
  (interactive 
  (perlnow-prompt-user-for-file-to-create 
   "Name for the new perl script? " perlnow-script-location))
  (perlnow-new-file-using-template filename perlnow-perl-script-template)
  (perlnow-change-mode-to-executable))


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

;;; Question: is there something intelligent that could be done 
;;; with EXPORT_OK to provide a menu of options here? 
;;; Should you qw() *all* of them (maybe except for :all) and let user delete?

;;; Maybe someday: check first PERL5LIB and don't add FindBin jazz if module 
;;; is already findable.  Easy enough: (getenv "PERL5LIB") Search through result 
;;; for matching (file-name-directory module-name).

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

;;; Maybe: include the old perlutil-* routines. 
;;; Detect if template.el is installed, and if not, 
;;; fall back on using these (instant gratification principle... 
;;; try and do something useful, even if the installation isn't 
;;; quite right... but notify somehow that there's a problem to 
;;; be fixed). 

;;;   (defun perlutil-perlnow ()
;;;   (defun perlutil-perlify-this-buffer ()


;;;==========================================================
;;; Internally used functions 
;;;==========================================================

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

  
(defun perlnow-new-file-using-template (filename template)
  "Given filename and template, does the actual creation of
the file and associated buffer using the template"

;;; Because of a bug in template.el, when using template-new-file 
;;; non-interactively, we must set the global "template-file" here:
  (setq template-file (template-split-filename filename)) 
  (template-new-file filename template)
  (write-file filename))


(defun perlnow-script-p ()
  "Determine if the buffer looks like a perl script by looking for the 
hash-bang line at the top."
  (save-excursion 
  (let (
        (hash-bang-line-pat "^[ \t]*#!.*perl\\b") ; note, presumes an explicit "perl"
        pee)
    (goto-char (point-min))
    (if (looking-at hash-bang-line-pat) 
        (setq pee 't)
      (setq pee 'nil))
    pee)))


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

;;; TODO - need an alternate (perhaps a replacement?) version of this that returns 
;;; the module-name.  NEEDS TESTING BADLY
(defun perlnow-get-module-name () 
  "Return the module name from the package line, or nil if there is none"
  (save-excursion 
  (let ((package-line-pat "^[ \t]*package[ \t]*\\(.*\\)[ \t;]")  ;;; got to add (Mod::Name) to this pat
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
        pee)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat) 
        (match-string 1)
      (nil)))))

(defvar perlnow-script-run-string nil "Default run string for perl scripts")
(defvar perlnow-module-run-string nil "Default run string for perl modules")
;(setq perlnow-script-run-string nil)
;(setq perlnow-module-run-string nil)

(defun perlnow-set-run-string ()
  "Prompt the user for a new string that perlnow-run will
use to run the code in the current buffer.  Sets the `compile-command'.\n
Don't use this from within a program: instead set the variables 
directly, see `perlnow-script-run-string', `perlnow-module-run-string'
and `compile-command'.\n

This function uses \\[perlnow-module-p] to see if the code looks like a
module (i.e. does it have a package line), otherwise it 
assumes it's a perl script.  If it's not perl at all,
that's your problem: the obvious tests for perl code, 
like looking for the hash-bang, aren't reliable (perl
code need not have a hash-bang line: e.g. *.t files,
windows perl...)."
  (interactive)
   (cond
   ((perlnow-module-p)
    (make-local-variable 'perlnow-module-run-string)
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
    ; tell emacs how to do it
    (setq compile-command perlnow-module-run-string))
   (t  ;if not a module, just assume it's a script.  Old way was this (unreliable) test:  ((perlnow-script-p)
    (make-local-variable 'perlnow-script-run-string)
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
    ; tell emacs to do it that way
    (setq compile-command perlnow-script-run-string))))

;;; confirm that "compile-command" is buffer local
(defun perlnow-run (&optional runstring) ;;; is the optional okay there?
  "Run the perl code using the either perlnow-script-run-string or 
perlnow-module-run-string, as appropriate"
  (interactive)
  (message "compile-command: %s" compile-command) ; debugging only  DELETE
  (compile compile-command))


;;;==========================================================
;;; Experimental code can go here     BOOKMARK (note: trying reg-*)

(defun perlnow-choose-module-run-string ()
  "Returns a good guess for an appropriate perlnow-module-run-string, determined like so:
\(1\) Looks up one level above the module-root, checks the name of
    this location, if it's the hyphenized form of the module name, 
    then that's the perl package staging-area. 
    \(a\) If there's a Makefile in the staging-area then use \"make test\".
    \(b\) If there's a Makefile.PL there, then do a \"perl Makefile.PL\" first. 
\(2\) If there's a subdir named \"t\" one-level up from the module root, 
    run any *.t files there as perl\n"
  (interactive) ;;; maybe just for debugging.  DELETE
  (let* ( (module-name (perlnow-get-module-name))
          (module-file-location (file-name-directory (buffer-file-name)))
          (module-root (perlnow-get-module-root module-name module-file-location ))
  
;;; BOOKMARK ;;; 


         )))
;;; Point (2) implies a *list* of shell commands to run, doesn't it? 
;;; Have code do a listp on the "run-string" to see what it really is?
;;; Could punt, and write it as an external file /tmp/New-Module-test-02383.sh
;;; Or could punter and just drop point (2) for now.  Except, it could also 
;;; be used on (2b)...

;;; TODO do this kind of stuff in the docs, where ever they may go.
;;; Definitions: 
;;; TODO - check perl definitions, e.g. precise distinction between package and module.
;;; module-file-name - the file system's name for the module file, e.g. /usr/lib/perl/Double/Colon.pm
;;; module-location  - directory portion of module-file-name, e.g. /usr/lib/perl/Double/
;;; module-name or package-name - perl's double colon separated name, e.g. "Double::Colon"
;;; module-root - The place where perl's double-colon name space begins (e.g. /usr/lib/perl)
;;;               The PERL5LIB environment variable is a list of different module-roots. 
;;; staging-area - the directory created by the h2xs command for module development, 
;;;                a "hyphenized" form of the module-name e.g. Double-Colon
;;; h2xs-location - the place where you put your staging-areas (possibly /home/doom/dev ?)


(defun perlnow-get-module-root (package-name module-location)
  "Given the module file location and the package name, determine module-root"
;  (interactive "sPackage Name: \nsModule Location: ") ;;; DEBUG only DELETE
;;; WORKS well enough.  TODO Cleanup names (function and variables)
  (let (double-colon-count file-levels)
    (setq double-colon-count (length (split-string package-name "::")))
    (setq file-levels (split-string module-location "/"))
    (setq module-root (mapconcat 'identity 
                                 (butlast file-levels double-colon-count)
                                 "/"))
    (setq module-root (concat "/" module-root)) ; kludge, prepends a "/" 
                                                ; (code will break if not given full-path)
    (message "Module root: %s" module-root)))

  ;;; Example: 
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/New/Module.pm 
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/New/          => number of levels:  7
  ;;;                                    New::Module   => double-colon-count: 1
  ;;;  /home/doom/Testes/Junk/Punk/Skunk/                 The desired module-root




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

(defun perlnow-blank-out-display-buffer (buffer)
  "Clear out a temporary display buffer, i.e. one whose 
name begins with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument can be a string or a buffer. 
This can work on a read-only buffer."
;;; TODO - this looks like it should be converted to cond
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
