;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.1 2004/01/17 09:10:34 doom Exp root $
;; Keywords: 
;; X-URL: not distributed yet

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

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlnow)
;;   (global-set-key  "\M-ps" 'perlnow-script)
;;   (global-set-key  "\M-pm" 'perlnow-module)
;;   (setq perlnow-script-location (concat (getenv("HOME") "/bin"))) ; ~/bin
;;   (setq perlnow-module-location (concat (getenv("HOME") "/lib"))) ; ~/lib
;; 

;;; Code:

(provide 'perlnow)
(eval-when-compile
  (require 'cl))

(require 'template)  ; templating system: easily customizeable initial file contents


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

; I'm grabbing "Alt p" as the perlnow prefix, but using the
; cperl-mode-map and perl-mode-map since perlnow doesn't
; have a map of it's own since it's not a mode.  

; Is Alt-p okay? (It *and* Alt-n are undefined by default, 
; but that suggests some intent to use them as customizeable 
; navigation bindings) 

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
   ; on the following two, I'm currently using HOME
   ; environment variable for a default location, though
   ; it's expected this will be overriden with a .emacs
   ; setting.  Maybe it would be better to default to
   ; something else, like ~/bin and ~/lib, but in that case
   ; would have to make sure they exist and create them
   ; otherwise.

(defvar perlnow-script-location (getenv "HOME")
    "The default location to stash new perl scripts")

(defvar perlnow-module-location (getenv "HOME")
    "The default location to stash new perl modules")


(defvar perlnow-executable-setting ?\110
   "Pattern of user-group-all permission settings used when making a script executable")

(defvar perlnow-perl-script-template (concat (getenv "HOME") "/.templates/TEMPLATE-perl.tpl")
   "The template.el template new perl scripts will be created with" )
;;; (setq perlnow-perl-script-template (concat (getenv "HOME") "/.templates/TEMPLATE-perl.tpl"))

(defvar perlnow-perl-module-template (concat (getenv "HOME") "/.templates/TEMPLATE-pm.tpl")
   "The template.el template new perl modules will be created with" )

(defvar perlnow-perl-module-name 'nil
  "Used internally to pass the new full module name in
double-colon separated form to the perl module template ")
;;; TODO - Look for a way to do this without a global variable

;;; Adding template feature PERL_MODULE_NAME for the perlnow-module function. 
;;; TODO - FIX something wrong with the syntax here? Single-Quote out of place maybe: "'(insert"
(setq template-expansion-alist 
        (cons 
          '("PERL_MODULE_NAME" . '(insert perlnow-perl-module-name) )
          template-expansion-alist))
;;;
;;; Eval this to erase effects of the above:
;;; (setq template-expansion-alist 'nil)

;;;==========================================================
;;; Macros
;;;==========================================================

(defmacro perlnow-require-trailing-slash (path)
  "Appends a slash to the end of string in variable, 
unless one is there already"
;;; TODO - if this needs to add a / it returns 't: 
;;;        would prefer it always return path 
   `(or (string-equal "/" (substring ,path (- (length ,path) 1)))
        (setq ,path (concat ,path "/"))))

;;;==========================================================
;;; User Commands
;;;==========================================================

(defun perlnow-run-check ()
  "Run a perl check on the current buffer."
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

   (perlnow-create-file-using-template filename perlnow-perl-script-template)

   (perlnow-make-executable)
   (perlnow-set-modes-for-perl))

;;; 

(defun perlnow-script-using-this-module (filename)
  "Quickly jump into writing a perl script that uses the module 
in the currently open buffer"
  (interactive 
    (perlnow-prompt-user-for-file-to-create 
      "Name for the new perl script? " perlnow-script-location))

  (let (package-name
        (module-filename (buffer-file-name))) 

    ;;; read the package name out of the initial module file buffer:
    (save-excursion 
      (let ((comment-line-pat "^[ 	]*$\\|^[ 	]*#")
            (package-line-pat "^[ 	]*package \\(.*\\)[ 	]*;"))
        (goto-char (point-min))
        (while (looking-at comment-line-pat) (forward-line 1))
        (if (looking-at package-line-pat)
            (setq package-name (match-string 1))
          (error "%s" "No leading package line: This file doesn't look like a perl module"))))
        (message package-name)  ;;; for debugging purposes

        ;;; create new script file buffer using template
        (perlnow-create-file-using-template filename perlnow-perl-script-template)

        ;;; insert the lib path tweaks to ensure the module can be found by the script
        (let ((relative-path
               (file-relative-name module-filename (file-name-directory filename))
               ))
          (insert "use FindBin qw\($Bin\);\n")
          (insert "use lib \(\"$Bin/")
          (insert relative-path)
          (insert "\");"))

        ;;; insert the "use Some::Module;" line
        (insert (format "use %s;" package-name)) ;;; and maybe a qw() list? 
        (insert "\n")))
;;; Question: is there something intelligent that could be done 
;;; with EXPORT_OK to provide a menu of options here? 
;;; Should you qw() *all* of them (maybe except for :all) and let user delete?
p
;;; Maybe someday: check first PERL5LIB and don't add FindBin jazz if module 
;;; is already findable.  Easy enough: (getenv "PERL5LIB") Search through result 
;;; for matching (file-name-directory module-name).

;;; TODO: Option to include the SYNOPSIS section (commented out)


;;;==========================================================
;;; functions to allow creation of new *modules* not scripts

;;; TODO:  would like to accept slash separated form of the name
;;;        as well as double colon notation.
;;;       (Appending *.pm should be allowed, but not required)

;;; Note: before you look into this stuff too much, see what h2xs does 
;;; *for* you.


;;;  BOOKMARK 
;;; TODO: STEP ONE: Just diagram this shit.  Why so many layers of function 
;;;    calls? 

(defun perlnow-module (module-location module-name) 
  "Quickly jump into development of a new perl module"
  (interactive 
     (perlnow-prompt-user-for-module-to-create (perlnow-module-location))

   (setq perlnow-perl-module-name module-name) ; global used to pass value into template

   (let ((filename (perlnow-full-file-name module-location module-name) ))
     (perlnow-create-file-using-template filename perlnow-perl-module-template))
   (perlnow-make-executable)
   (perlnow-set-modes-for-perl)))

;;; (TODO - Maybe move these down to the Internal section when they're finished)

(defun perlnow-prompt-user-for-module-to-create (default-location) 
  "Ask the user two questions: the location and the name of the perl 
module to create, check to see if one exists already, and if so, 
ask for another name.  Returns a two element list, location and module-name."

; save current directory (save-excursion?)
; change current directory to the default location (so D use it)
; Call function to ask: Where should the module go? 
; Call function to ask: What is the name of the module? 

  (perlnow-require-trailing-slash default-location)
;;; TODO Look into the right way to undo the change to default-directory 
;;; later... use a "let" to set it? (Does that work without lexicals?)
;;; Just save the value and restore later?  At least that's guaranteed to work.
  (setq default-directory default-location)
  (let ((where (call-interactively 'perlnow-ask-where))
        (what  (call-interactively 'perlnow-ask-what)))
        (list where what)))

(defun perlnow-prompt-user-for-module-to-create-stage-two (where what)
   "Doing this in two stages, because interactive \"D\" uses the 
default-directory as it's default, so that has to be set *before* 
calling interactive \"D\""
;;; Trying to piece together ask-where and ask-what into one routine.
;;;  BOOKMARK - TEST THIS

  (interactive "D:Where? \ns:What? ")
  (list where what))
  
(defun perlnow-ask-where (where)
  (interactive "D:Where? ")
  (message where))

(defun perlnow-ask-what (what)
  (interactive "s:What? ")
  (message what))

(defun perlnow-test (where what)
  (interactive (perlnow-prompt-user-for-module-to-create "/home/doom/tmp"))
  (message "where: %s, what: %s" where what))

;;; TODO - Above works but should be cleaned up.  
;;;   (1)  Possible for one defun to do a ask-where-and-what? 
;;;        Note, need to return two values. (Better prompts, too, explain 
;;;        need to use perl's internal module name form, Blah::Bleh not Blah/Bleh.pm)
;;;   (2)  Note, as written directory must exist, *but* subdirs implied by the 
;;;        module name (e.g. "Blah") might need to be created.  Do that. 

(defun perlnow-full-file-name (module-location module-name)
  "Piece together a location and a perl-style module name into a full file name: 
given \"/home/doom/lib\" and \"Text::Gibberish\" would yield /home/doom/lib/Text/Gibberish.pm"
  (let ((filename 
         (concat 
          (mapconcat 'identity (split-string module-name "::") "/")
            ".pm")))
     (perlnow-require-trailing-slash module-location)
     (concat  module-location filename)))

(defun perlnow-anothertest () 
  (interactive)
  (message "full file name: %s" (perlnow-full-file-name "/home/doom/lib/" "HaHa")))






;;;==========================================================
;;; Older code 
;;;==========================================================

;;; Maybe: include the old perlutil-* routines. 
;;; Detect if template.el is installed, and if not, 
;;; fall back on using these?  

;;;   (defun perlutil-perlnow ()
;;;   (defun perlutil-perlify-this-buffer ()


;;;==========================================================
;;; Internally used functions 
;;;==========================================================

(defun perlnow-make-executable ()
   "Makes the file associated with the current buffer executable"

  ; Save first, to try to make sure the file really exists before
  ; we change the protections on it ((better ways?))
  (save-buffer)

  (let ((all-but-execute-mask ?\666)
        (file-permissions)
        (new-file-permissions))

  (setq file-permissions (file-modes (buffer-file-name)))
  (setq new-file-permissions 
    (+ (logand file-permissions all-but-execute-mask) perlnow-executable-setting))
  (set-file-modes (buffer-file-name) new-file-permissions)))


(defun perlnow-set-modes-for-perl ()
   "set modes for perl code display "
   ; Changes a (presumeably newly created) file buffer to 
   ; modes suitable for displaying perl code: cperl-mode with font-lock-mode. 
   ; Yes, this is presumptious:
   ; The Emacs Way would probably be to look at interpreter-mode-alist
   ; and infer from that what the user would prefer.   (Unless there's 
   ; some general command you can run that'll check that alist for you 
   ; and figure it's perl from the hashbang line or the *.pm (or .pl?)
   (cperl-mode)  
  ; Turn on font-lock-mode, (if not on already) Why not "if
  ; *not* font-lock"?  Because this works. Dunno why.
  (if (font-lock-mode) (font-lock-mode)) )


(defun perlnow-prompt-user-for-file-to-create (ask-mess default-location) 
  "Ask the user for the name of the script to create,
check to see if one exists already, and if so, ask for another name.  
Returns full file name with path."
;;; TODO - add feature to check if intervening directories exist and create them if needed

  (let ( filename )
    (perlnow-require-trailing-slash default-location) ;;; would rather do this just once.
    (while (progn 
             (setq filename 
                   (expand-file-name
                   (read-file-name ask-mess default-location)))
             (setq ask-mess 
                   "That name is already in use, please use another name: " )
             (file-exists-p filename)))
      (message filename) ;;; CHECK: must be better way to get it to return filename (eval?)
      ))

(defun perlnow-create-file-using-template (filename template)
  "Given filename and template, does the actual creation of
the file and associated buffer using the template"
;;; Because of a bug in template.el, when using template-new-file 
;;; non-interactively, we must set the global "template-file" here:
    (setq template-file (template-split-filename filename)) 
    (template-new-file filename template))

;;;===========================================================================
;;;  History 
;;;===========================================================================

;; This began life as excerpts from "perlutil.el" (not distributed).  
;; It was time to clean-up some cruft now that I was settling on  
;; using template.el templating, and I wanted to do a re-name in anycase 
;; ("perlutil" already means something else in the perl world, "man perlutil" 
;; will tell you about utilities that come with perl).

;; The main concept is to make it easy to quickly jump into perl development.
;; It's first incarnation was a keyboard macro that insert the hashbang line. 
;; Then came some elisp that could do useful things like make the new file 
;; executable, and insert a standard header with title, email and date. 
;; Then I wanted to get away from the hard-coded header, so I started 
;; experimenting with templating systems -- following the golden (silver? 
;; well, at least tin) rule: "though shalt not invent yet another templating 
;; system" I started looking at the various existing lisp packages.  
;; The tempo.el packaged had some promise, but also some problems and it's largely
;; designed for other things (heavily interactive uses, like writing new major 
;; modes).  I've settled on template.el as being closest to what I want 
;; though it's more complicated than what I need, probably isn't widely 
;; distributed and I've had to kludge a work-around in an existing bug 
;; (which I will report someday if I can understand it well enough... 
;; like I said, excessively complicated). 

;; See here for more:
;;   /home/doom/End/Hack/Emacs/notes-perlutil

;;; perlnow.el ends here
