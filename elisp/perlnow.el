;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.3 2004/01/19 09:27:59 doom Exp root $
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
;;   (setq perlnow-script-location (substitute-in-file-name "$HOME/bin"))
;;   (setq perlnow-module-location (substitute-in-file-name "$HOME/lib"))
;; 

;;; Code:

(provide 'perlnow)
(eval-when-compile
  (require 'cl))

(require 'template)  ; templating system: easily customizeable initial file contents


;;;==========================================================
;;; Macros
;;;==========================================================

(defmacro perlnow-require-trailing-slash (path)
  "Appends a slash to the end of string in variable, 
unless one is there already"
   `(or (string-equal "/" (substring ,path (- (length ,path) 1)))
        (setq ,path (concat ,path "/"))))
;;; TODO - it turns out that this existing function does this already: 
;;; (setq slashed-dirname (file-name-as-directory dirname)) 


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
   ; on the following two, I'm currently using HOME environment variable for
   ; a default location, though it's expected this will be overriden with a
   ; .emacs setting.  Maybe it would be better to default to something else,
   ; like ~/bin and ~/lib, but in that case would have to make sure they
   ; exist and create them otherwise. 
   ; Or see if they exist, and then use them, if not, silently fall back on HOME?

(defvar perlnow-script-location (getenv "HOME")
    "The default location to stash new perl scripts")
(perlnow-require-trailing-slash perlnow-script-location)

(defvar perlnow-module-location (getenv "HOME")
    "The default location to stash new perl modules")
(perlnow-require-trailing-slash perlnow-module-location)

(defvar perlnow-executable-setting ?\110
   "Pattern of user-group-all permission settings used when making a script executable")

(defvar perlnow-perl-script-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.perl.tpl"))
   "The template.el template new perl scripts will be created with" )
;;; DELETE:
;;; (setq perlnow-perl-script-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.perl.tpl"))

(defvar perlnow-perl-module-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.pm.tpl"))
   "The template.el template new perl modules will be created with" )
;;; DELETE:
;;; (setq perlnow-perl-module-template (substitute-in-file-name "$HOME/.templates/TEMPLATE.pm.tpl"))

(defvar perlnow-perl-module-name 'nil
  "Used internally to pass the new full module name in
the perl double-colon separated form to the module template ")
;;; TODO - Look for a way to do this without a global variable

;;; Adding template feature PERL_MODULE_NAME for the perlnow-module function. 
(setq template-expansion-alist 
        (cons 
          '("PERL_MODULE_NAME" (insert perlnow-perl-module-name) )
          template-expansion-alist))

;;; DELETE ME
;;; Eval this to erase effects of the above:
;;; (setq template-expansion-alist 'nil)
;;; END DELETIA

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

;;;

(defun perlnow-script (filename)
  "Quickly jump into development of a new perl script"
  (interactive 
     (perlnow-prompt-user-for-file-to-create 
       "Name for the new perl script? " perlnow-script-location))
   (perlnow-new-file-using-template filename perlnow-perl-script-template)
   (perlnow-change-mode-to-executable)
   (perlnow-set-emacs-modes-for-perl))

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

;;; TODO: Option to include the SYNOPSIS section (commented out)


;;;==========================================================
;;; functions to allow creation of new *modules* not scripts

;;; TODO:  would like to accept slash separated form of the name
;;;        as well as double colon notation.
;;;       (Appending *.pm should be allowed, but not required)

;;; Note: before you look into this stuff too much, see what h2xs does 
;;; *for* you.


;;; BOOKMARK -- This is very close, but there's an error somewhere, maybe in:
;;; perlnow-new-file-using-template

(defun perlnow-module (module-location module-name) 
  "Quickly jump into development of a new perl module"
  (interactive 
     ; save and restore default-directory (using let dynamic scoping magic)
        (let* ((default-directory perlnow-module-location))
          (call-interactively 'perlnow-prompt-for-module-to-create)))

   ;;; DELETE ME  - look at *Messages* make sure this looks right.
   (message "default directory has returned to:%s" default-directory)
   ;;; END DELETIA

   (setq perlnow-perl-module-name module-name) ; global used to pass value into template
   (let ( (filename (perlnow-full-path-to-module module-location module-name)) )
     
     (perlnow-new-file-using-template filename perlnow-perl-module-template))
   (perlnow-set-emacs-modes-for-perl))


(defun perlnow-prompt-for-module-to-create (where what) 
  "Ask the user two questions: the location and the name of the perl 
module to create, check to see if one exists already, and if so, 
ask for another name.  The location defaults to the current default-directory.
Returns a two element list, location and module-name."
  (interactive "D:Location for new module? \ns:Name of new module \(perl-style, e.g. Blah::Bleh\)? ")
  
  (let* ((filename (perlnow-full-path-to-module where what))
         (dirname (convert-standard-filename (file-name-directory filename))))
; DELETE
; For some reason this carefully crafted code is not necessary: 
; something else is prompting for directory creation, if needed. 
; 
;    (if (not (file-exists-p dirname)) ; check existance of directory. 
;        (if (y-or-n-p (format "Path %s not found. Create it?" dirname))
;           (make-directory dirname 't)
;         (error "Some part of path %s doesn't exist, can't create module." dirname)))
;    (if (not (file-accessible-directory-p dirname)) 
;        (error "Permissions problem on location %s, can't create module." dirname))
; END DELETE
    )
  (list where what))

;;; TODO - o  needs testing
;;;        o  consider returning "filename" also, to avoid piecing it together again later 
;;;           (but efficiency tweaks are low priority)

(defun perlnow-full-path-to-module (module-location module-name)
  "Piece together a location and a perl-style module name into a full file name: 
given \"/home/doom/lib\" and \"Text::Gibberish\" would yield /home/doom/lib/Text/Gibberish.pm"
  (let ((filename 
         (concat 
          (mapconcat 'identity (split-string module-name "::") "/")
            ".pm")))
     (perlnow-require-trailing-slash module-location)
     (concat  module-location filename)))


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
  (insert " ") 
  (delete-backward-char 1) 
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


(defun perlnow-set-emacs-modes-for-perl ()
   "set modes for perl code display "
   ; Changes a (presumeably newly created) file buffer to emacs 
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


;;;==========================================================
;;; Experimental code can go here     BOOKMARK (note: trying reg-*)

(defun stoopid () 
  "Stoopid!"
  (interactive)
  (let ( (name1 "/usr/lib/emacs/site-lisp/perlnow.el")
         (name2 "/usr/lib/emacs/site-lisp")
         (name3 "/home/doom/tmp/Grossmiller/Fook")
         (name4 "/tmp/no-way-in-hell")
         (name5 "/home/doom/tmp")
         )

     (if (file-exists-p name5)
         (message "file-exists pee: %s" name5))

    (if (not (file-exists-p name3))
        (message "here we is, no %s is there?" name3))
    ))
;    
;(file-accessible-directory-p dirname) ; makes sure you can write to directory 
;(file-directory-p filename)           ; makes sure file is a directory
;
;(file-exists-p filename)    ; does this return 't for a directory? (Yes can check dir exist with this)
;(file-readable-p filename)
;(file-writeable-p filename)




;;;===========================================================================
;;;  History 
;;;===========================================================================

;; This began life as excerpts from "perlutil.el" (not distributed).  
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

;;; perlnow.el ends here
