;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code. 

;; Copyright 2004 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.113 2004/02/18 20:14:07 doom Exp root $
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
;; 
;;  perlnow.el is intended to speed the development of perl code 
;;  by automating some routine tasks.
;;
;;  See the documentation for the variable perlnow-documentation, 
;;  and it's relatives, below.

;;; Code:
(provide 'perlnow)
(eval-when-compile
  (require 'cl))

(defconst perlnow-version "0.1"
  "Current version of package perlnow.
Check <http://www.grin.net/~mirthless/perlnow/> for the newest.")

(defvar perlnow-documentation t 
 "The introductary documentation to the perlnow.el package.
Also see the documentation for:
`perlnow-documentation-installation'
`perlnow-documentation-terminology'
`perlnow-documentation-template-expansions'
`perlnow-documentation-tutorial'
`perlnow-documentation-test-file-strategies'
 
This package is intended to speed development of perl code,
largely by making it easier to jump into coding when an idea
strikes. It also includes some commands to help automate some routine
development tasks including testing the code in the emacs environment. 

A perlnow command will typically prompt for a location and/or name, 
open a file buffer with an appropriate framework already inserted 
\(e.g. the hash-bang line, comments including date and author 
information, a perldoc outline, and so on\).  In the case of scripts 
the file automatically becomes executable. 

Many of the perlnow.el features require that template.el
package has been installed, along with some templates for
perl development purposes. See `perlnow-documentation-installation'.
 
Primarily, perlnow.el provides the following interactive 
functions: 
 
\\[perlnow-script] - for creation of new perl scripts.  If
currently viewing some perl module code or a man page for a
perl module, this begins the script with a few lines to use
the module.

\\[perlnow-script-simple] - an older, not quite deprecated
form of \\[perlnow-script] that has the virtue of not
needing template.el to operate.

\\[perlnow-module] - for creation of new modules.  Asks for
the location and name of the new module in a single prompt,
using a hybrid form: \"/usr/lib/perl/Some::Module\"

\\[perlnow-h2xs] - runs the h2xs command, to begin working
on a new module for distribution, such as via CPAN.

\\[perlnow-run-check] - does a perl syntax check on the
current buffer, displaying error messages and warnings in
the standard emacs style, so that the next-error command,
usually bound to \"C-x \`\" \(control-x back-apostrophe\)
will skip you to the location of the problem.

\\[perlnow-run] - like the above, except that it actually
tries to run the code, prompting the user for a run string
it if it has not been defined yet.

\\[perlnow-set-run-string] - Allows the user to manually
change the run-string used by perlnow-run.

\\[perlnow-perldb] - runs the perl debugger using the above run string.

A list of the important functions that require template.el: 
\\[perlnow-script]
\\[perlnow-module]
\\[perlnow-module-two-questions]
 
Many useful functions here don't need template.el.
Briefly these are: 
\\[perlnow-run-check]
\\[perlnow-run]
\\[perlnow-set-run-string]
\\[perlnow-h2xs]
\\[perlnow-script-simple] \(older alternate to \\[perlnow-script]\)
\\[perlnow-perlify-this-buffer-simple] \(an even older alternate\)")

(defvar perlnow-documentation-installation t
  "Instructions on installation of the perlnow package.

Put the perlnow.el file somewhere that's included in your `load-path'.

Also install template.el if at all possible, because
many \(but not all\) features of this package depend on
template.el.  The latest version can be found at:

   http://sourceforge.net/project/showfiles.php?group_id=47369

In addition, you'll need some custom perl-oriented
template.el templates that come with perlnow.el.  Most
likely these templates should go in your ~/.templates,
\(note they end with: '.tpl'\).  If you've somehow obtained
the perlnow.el file without the associated templates, 
you can look for copies of them here:

   http://www.grin.net/~mirthless/perlnow/

Add something like the following to your ~/.emacs file:

   \(require 'template\)
   \(template-initialize\)
   \(require 'perlnow\)
   \(global-set-key \"\\C-c=s\" 'perlnow-script\)
   \(global-set-key \"\\C-c=m\" 'perlnow-module\)
   \(global-set-key \"\\C-c=h\" 'perlnow-h2xs\)
   \(global-set-key \"\\C-c==\" 'perlnow-run-check\)
   \(global-set-key \"\\C-c=r\" 'perlnow-run\)
   \(global-set-key \"\\C-c=d\" 'perlnow-perldb\)
   \(global-set-key \"\\C-c=c\" 'perlnow-set-run-string\)
   \(global-set-key \"\\C-c=b\" 'perlutil-perlify-this-buffer\)
  \(setq `perlnow-script-location' 
      \(substitute-in-file-name \"$HOME/bin\"\)\)
  \(setq `perlnow-module-location' 
      \(substitute-in-file-name \"$HOME/lib\"\)\)\n
  \(setq `perlnow-h2xs-location'' 
      \(substitute-in-file-name \"$HOME/perldev\"\)\)\n

Some suggestions on key assignments: 

Here I'm using the odd prefix \"control-c =\", simply
because while the perlnow.el package is not a minor-mode, it
has some aspects in common with them \(and maybe it's on
it's way to becoming one\).  The C-c <punctuation> bindings
are the only places a minor mode is supposed to mess with
the keymap \(and at least \"=\" is un-shifted on most
keyboards\).  You, on the other hand, are free to do whatever
you want in your .emacs, and I would suggest assigning the
commands you like to function keys.  Some examples from my
.emacs:

  \(global-set-key [f4] 'perlnow-script\)

  \(add-hook 'cperl-mode-hook
          '\(lambda \(\) 
             \(define-key cperl-mode-map [f1] 'perlnow-perl-check\) \)\)

Note: perlnow.el was developed using GNU emacs 21.1 running
on a linux box \(or GNU/Linux, if you prefer\).  I've
avoided using constructs that I know won't work with earlier
versions of emacs, and I don't know of any reason it
wouldn't work with xemacs, but none of that has been tested.
On the other hand, I'm pretty sure that some unix-isms have
crept into this code: for example, if your file-system
expects a \"/\" as a separator between levels, this package
may have some problems.  I'm ammenable to suggestions for
ways to make future versions of this more portable.")

(defvar perlnow-documentation-terminology t 
"Definitions of some terms used here: 

Note: This documentation \(and some of the code\), makes the
simplifying assumption that a perl package is a perl module
is a single file, \(with extension *.pm\).  Even though
technically multiple packages can occur in a single file,
that is almost never done in practice.

Why is there such a mess of terminology here?
Because there's a file system name space and a module name space:

   /usr/lib/perl/Some/Module.pm
   /usr/lib/perl/Some::Module

This makes the answers to simple questions ambiguous:

What is the module called?  
  Module.pm
  Some::Module

Where is the module? 
  /usr/lib/perl/Some
  /usr/lib/perl

The following terms are used here in an attempt at being
more precise:

PM FILE \(or MODULE FILENAME\): the file system's name for
the module file, e.g. /usr/lib/perl/Some/Module.pm

MODULE FILE BASENAME: name of the module file itself, sans
extension: in the above example, \"Module\"

MODULE LOCATION \(or MODULE FILE LOCATION\): directory
portion of module file name, e.g. /usr/lib/perl/Some/

MODULE NAME or PACKAGE NAME: perl's double colon separated
name, e.g. \"Some::Module\"

INC SPOT: a place where perl's package space begins
\(e.g. /usr/lib/perl\). Perl's @INC is a list of different
such \"inc spots\" \(alternate term: \"module root\" or
\"package root\"\).

STAGING AREA: the directory created by the h2xs command
for module development, a hyphenized-form of the module name
e.g. Some-Module.  Every staging area contains a module root
\(or \"inc spot\") called \"lib\".

H2XS LOCATION: the place where you put your staging areas

PERLISH PATH: this means a module path including double
colons \(alternate term: \"colon-ized\"\),

FILE SYSTEM PATH \(or FILESYS PATH\): as opposed to
\"perlish\".  This is the regular \'nix style slash
separated path.

FULL: usually meaning that the full path is included,
e.g. \"full file name\".

TEST SCRIPT: The *.t file associated with the current
module/script\(?\), usually something like ModuleName.t or
possibly Staging-Area.t.

TEST LOCATION: place where the test script\(s\) are for
a given module/script

TEST PATH: search path to look for test files. Note, can
include relative locations, e.g. \"./t\", but the the dot
there shouldn't be taken as simply the current
directory... See: `perlnow-test-path'.

TEST POLICY: the information necessary to know where to
put a newly created test file \(\( *not yet implemented*
\)\): 1 - the test path dot form, e.g. \"./t\" 2 - the
definition of dot e.g. module-file-location vs. inc-spot 3 -
the naming style, e.g. hyphenized vs. base.")

(defvar perlnow-documentation-tutorial t
  "Well, first you install it: `perlnow-documentation-installation'.
Then what? 

Depending on how you config things, you should then have
easy access (perhaps as easy as a single keystroke of a
function key) to some quick short-cuts.  Here's a run down 
on how you might use them for different purposes:

 `perlnow-documentation-tutorial-1-script-development'
 `perlnow-documentation-tutorial-2-module-development'
 `perlnow-documentation-tutorial-3-h2xs-module-development'
 `perlnow-documentation-tutorial-4-misc'
 `perlnow-tutorial-test-file-strategies'")

(defvar perlnow-documentation-tutorial-1-script-development t
  "Got an idea for a script?  Hit \\[perlnow-script].

This will ask you for the name of the script you want to
write, then kick you into a file buffer with a code template
set-up already.  

If you don't like the template, change it \(it should be in
your ~/.templates directory\).  For example, you might
prefer to have \"use strict;\" appear commented out but
ready to be enabled when you know the script is going to be
longer than a dozen lines.

Currently perlnow--script tends to want to put all of your
new scripts in one place, the `perlnow-script-location' that
you've defined for it.  You can, of course, choose a
different place to put a script at creation time, and you'll
notice that the default is avaliable as a starting point to
edit into some new location, plus you've also got access to
the minibuffer history as well to get other starting places.

\(By the way: you do know about the minibuffer history,
don't you?  I didn't until recently.  During a minibuffer
read, you can step back and forth through the history of
things you've entered using: \\[previous-history-element]
and \\[next-history-element]]. Typically these are bound to 
Alt-p and Alt-n.\)

But every time you use \\[perlnow-script] it's going to try
and put it in the same default location, so \(a\) try and
pick a good default, and \(b\) think about changing it on
the fly if you're going to do a lot of work in a different
place.  You can use \\[set-variable] to set
`perlnow-script-location'.

Okay, so once you're in your new perl script buffer, you can
start coding away.  At any time, you can do a perlnow-run-check 
to make sure your syntax is okay.

Note that if you take nothing else away from messing with
the perlnow.el package, you owe it to yourself to grab this
perlnow-run-check command.  Don't get hung-up on any
installation hassles you might run into, don't tell yourself
\"maybe I'll play with that someday after I finish reading
all that long-winded documentation\", if need be just grab
that half-dozen lines of elisp and cut and paste it into
your .emacs.  If you haven't messed with something like this
before, you will be stunned and amazed at the convenience of
coding inside of emacs.  All perlnow-run-check does is act
as a wrapper around the emacs compile-command facility,
feeding in the \"perl -cw\" command.  Once you do the check,
the errors and warnings will be listed in another buffer,
and doing a \"next-error\" will rotate you through these,
skipping you directly to the point in the code where the
problem was reported.  Typically you run \"next-error\" 
with a control-x back-apostrophe, randomly enough.
It looks like your binding is: \\[next-error]

But as cool as \\[perlnow-run-check] is, you could skip it if
you like, and go straight to \\[perlnow-run], which will
\(most likely\) then ask you how you want to run the script.
The default command line is just \"perl <scriptname>\";
but you can append whatever arguments and re-directs you
like.  Once a run-string is defined for that file buffer
it will stop asking you this question, though you can change
the run string later at any time with \\[perlnow-set-run-string].

Everytime you do a \\[perlnow-run] it behaves much like doing a 
\\[perlnow-run-check]: any problems will be reported in another 
buffer, once again letting you do the \\[next-error] trick to 
jump to where you need to be.

By the way, you might notice I've said nothing about
stopping to do a \"chmod u+x\" to make the script
executable.  That's because \\[perlnow-script] does this for you.
Admittedly, this feature is less impressive than it used to
be in these emacs 21 days, when you can just put this in
your .emacs:

  \\(add-hook 'after-save-hook 
    'executable-make-buffer-file-executable-if-script-p\\)

When you run into a problem nasty enough to want to use the
debugger, I suggest using \\[perlnow-perldb], rather than
\\[perldb] directly.  The perlnow wrapper uses the
`perlnow-run-string' you've defined, which will be different
for each script.  If you use the perldb command directly,
you'll notice that the default is just however you ran it
last.  If you're switching back and forth between working on
two scripts, that default is going to be wrong a lot.

The next subject, developing perl modules: 
  `perlnow-documentation-tutorial-2-module-development'")

 
(defvar perlnow-documentation-tutorial-2-module-development t
   "When you're interested in writing a module, the proceedure 
is similar to script development: 
  `perlnow-documentation-tutorial-1-script-development'

The command you'll probably want to use is \\[perlnow-module], 
which will prompt you for a module location \(really, a \"module root\" 
location, or \"inc spot\", see `perlnow-documentation-terminology')
with a default of `perlnow-module-location' which should be set in 
your .emacs as indicated in `perlnow-documentation-installation' and 
can also be modified on the fly with \\[set-variable].  

Also, \\[perlnow-module] will ask for the name of the module, expecting 
it to be in perl's double-colon separated package name notation 
\(e.g. Double::Colon-ized\).  Interestingly enough, it gets the 
answer to both questions in a single mini-buffer prompt, letting you 
enter the answer in a hybrid form of file system path followed by 
module name.  Tab and space completion more or less works while 
you're navigating the previously existing part of the path.  When 
you hit enter, it will create whatever intervening directories 
it needs, after first prompting to make sure it's okay \(note, 
I'm a little dubious of that prompt, and I may get rid of it in 
the future\).  

Now, I have worked long and hard on getting this single-prompt
method of entering this information, and I'm very proud of
it, and I think it's wonderfully elegant, so the way
these things go the odds are good that you will hate it.

If so, you can use the older form of this command, which I've left 
in the code as \\[perlnow-module-two-questions].  It gets the 
same information, but does it by asking a separate question for 
where and what.  Auto-completion works on the \"where\" question, 
but not at all for the module name.

Note that one of the advantages of the \\[perlnow-run-check]
command for doing syntax checks is that it works on module
code just as well as on scripts: you don't need to have a
method of running the code to start getting syntax bugs out
of it.

If you do a \\[perlnow-run] it will \(a\) perform an elaborate 
search to try and find a test file for the module then \(b\) ask 
you for the name of a script to run that uses the module.  Unless 
you're some kind of sick and twisted extreme programming freak, 
the odds are pretty good you won't have either, yet.  In which 
case, you may want to use this command:

   \\[perlnow-script-using-this-module]

\(But by the way, if you *are* a test-first-code-later fanatic,
take a look at see `perlnow-tutorial-test-file-strategies'\)

Anyway, \\[perlnow-script-using-this-module] will get you
started writing a script that has a \"use <module name>\"
line inserted already.  If the module is not in your @INC
search path, it will also add the necessary \"FindBin/use
lib\" magic to make sure that the script will be able to
find the module.

If you skip back to the original module buffer, and do a \\[perlnow-run], 
you'll notice that the script you just created has become the default 
for the way the code in the module gets run. 

Another little gimmick hidden away here, is that
\\[perlnow-script-using-this-module] tries to snag the the
name of whatever perl \"sub\" the cursor happens to be near,
and pushes it on the kill-ring.  You can do a \\[yank] if
you've got some use for it.

But remember in order for that sub to be accessible, you
might need to do some chores like add the sub name to the
module's EXPORT_TAGS list, and then add it to a qw() list
appended to the \"use <package-name>\" inside the
script.

Currently the perlnow.el package is a little light on
features to smooth/sleaze your way past those obstacles \(we
do Have Plans, however\), but you might like to know that
the module template provided with perlnow puts some useful
locations in the numeric registers.  So you can quickly jump
to these positions with the emacs command
\\[jump-to-register], e.g. \(presuming the default
bindings\), doing a \"control x r j 9\" will take you to the
point stored in register 9.

Here's the count-down: 

   register    position
   9           EXPORT_TAGS
   8           EXPORT 
   7           SYNOPSIS
   6           DESCRIPTION

Next, the h2xs approach to module development: 
  `perlnow-documentation-tutorial-3-h2xs-module-development'")

(defvar perlnow-documentation-tutorial-3-h2xs-module-development t
  "There's another completely different style of perl module development, 
from the one discussed in: `perlnow-documentation-tutorial-2-module-development';
the h2xs module approach, which is intended to be used for modules 
which will be published on CPAN.  This of course, involves using the
standard framework created by the h2xs command, and for your
convenience the perlnow package provides: \\[perlnow-h2xs]. 

This will ask you two questions, \(1\) where do you want to
put the staging area that h2xs creates, and \(2\) what do you 
want to call this module.  The first question defaults to the 
customizeable variable `perlnow-h2xs-location'

\(Aside: my feeling is that asking two questions for the
creation of an h2xs structure, vs. the one question hybrid
form used by \[perlnow-module] is okay.  It helps
differentiate it from \[perlnow-module], and in any case it
doesn't logically lend itself to a single question form.  In
the case of h2xs the \"where?\" is the staging-area, not the
inc-spot.  The inc-spot is located inside a \"lib\" 
directory inside the staging-area, so there's a gap between 
the \"where\" and the \"what\", and we might as well represent 
that gap as the gap between the two questions.\)

Anyway, after you answer the two questions, \[perlnow-h2xs] 
will run the h2xs command, and then leave you with two windows 
open, one showing the module file buffer, the other showing the 
test file for the module. 

One of the nice features of the h2xs style of development is
the standard test framework.  These days this is implemented
using Test::More, and you should familiarize yourself with
the documentation for that.  

If you do a \\[perlnow-run] inside of an h2xs module, 
it should identify it as h2xs, and use \"make test\" as the 
run string.  \(Though actually, the first time you do this, 
it should notice that \"perl Makefile.PL\" hasn't been run 
yet, and do that first.\).

Next, everyone's favorite subject, \"Misc\":
 `perlnow-documentation-tutorial-4-misc'
")


(defvar perlnow-documentation-tutorial-4-misc t
  "Misc topic 1 - starting from man:

A typical 'nix-style box these days will have the documentation for 
perl modules installed as man pages, which can be most simply read 
from inside of emacs with the \\[man] command.  

  If you happen to be browsing some perl module
documentation in an emacs man window, you might suddenly be
struck by the urge to try it out in a script.  If so you
should know that the \\[perlnow-script] command is
smart enough \(*knock* *knock*\) to pick out the module name
from a man page buffer. This should kick you into a script 
template with the \"use <package-name>\" line already filled in.

\(By the way, the perldoc.el package looks like a promising
alternative to running \\[man], but it seems to just act as
a front-end to the man command... since you end up in the 
same kind of buffer, the \\[perlnow-script-using-this-module]
will work with that also.\)

Misc topic 2 - perlify:

In addition to the old-style non-templat.el fallback:
\\[perlnow-script-simple], there's another command 
something like it: \\[perlnow-perlify-this-buffer-simple].
The \"perlify\" concept is that you can use whatever 
habits you're used to to begin working on a script \(e.g. 
go into dired, choose a directory, choose a file name 
and open it there\), and *then* you can insert a simple 
code template and make the file executable.    

Originally I found that approach to be a little eaiser to get
used to than the \\[perlnow-script] approach, but 
pretty quickly I abandoned it and switched over.  Still, someday 
I expect to implement a template.el based \"perlify\", and 
the older version is available to do with what you will.")



(defvar perlnow-documentation-test-file-strategies t
  "As mentioned in a few other places, the \\[perlnow-run]
and \\[set-perlnow-run-string] commands do try to find 
test files for modules, even if they don't happen to be 
inside of an h2xs structure.  There's a relatively elaborate 
search path for this.  Here's a quick description of what it 
looks for before giving up and prompting the user 
 \(but please, avoid relying on the precedence of this 
search as currently implemented: it may change\): 

First of all, test files all end with the \".t\" extension 
\(just as with h2xs test files\).  There are two possibilities 
for the name of the basename of the test file, \(1\) it might 
just be the same as the base name for the \".pm\" file itself, 
or it might be a \"hyphenized\" form of the module's package 
name \(like an h2xs staging area name\).  For example, in the 
case of \"Modular::Silliness\", the name might be \"Silliness.t\", 
or \"Modular-Silliness.t\".

Secondly, a test file might be located in the same place
that a module file is located, or it may be located in the
module root location where the module's package name space
starts, or it might be tucked away in a directory called
\"t\" which could be located in either of those places. 

This means that there are a number of strategies you might
choose to use for your perl modules that should work well
with perlnow.el. \(And some of them are even reasonable. 
And some of them are already in use in industry.  And there's 
even some overlap between those two sets.\)

An example of a good practice would be to always use the hyphenized 
base name form, and always put test files in a subdirectory called 
\"t\" in the same place that the \".pm\" file is located.  

So if you've got a module called \"Modular::Silliness\", which 
is really the file: ~/perldev/lib/Modular/Silliness.pm 
For a test file, you could use:

  ~/perldev/lib/Modular/t/Modular-Silliness.t

If you don't like that you can use any of these schemes:

  ~/perldev/lib/t/Modular-Silliness.t
  ~/perldev/lib/Modular/t/Silliness.t
  ~/perldev/lib/Modular-Silliness.t
  ~/perldev/lib/Modular/Silliness.t

The ones you probably don't want to use are these :
  ~/perldev/lib/t/Silliness.t
  ~/perldev/lib/Silliness.t

\(There's too much potential for name collisions, if you use
the short \"basename\" form high up in the tree. Modular::Silliness 
and Monolithic::Silliness would fight to use the same name.\)

Note that perlnow \(at least currently\) does not care if you're 
consistent about this choice, but for your own sanity you should 
probably pick a standard way of doing it and stick to it.

An obvious next step for future developments in perlnow is to implement 
a set of \"create test file\" commands, which will require that the user 
define a policy that dictates where test files should go.  See \"test 
policy\" in `perlnow-documentation-terminology'.")

(defvar perlnow-documentation-unashamed-deviancy t 
  "There are a number of areas where I'm aware of deviating from 
standard andf/or recommended practice.  In a vain attempt 
at forestalling criticism, I'm going to list them: 

On variables such as `perlnow-script-run-string', I've 
used \\[make-variable-buffer-local] in preference to the 
recommended \\[make-local-variable].  I personally always 
want these variables to be buffer local, and I have trouble 
thinking of a reason that the user would want them otherwise.
It's much more convienient to use make-variable-buffer-local
right after they're defined, and to not have to worry about it 
later.

In minibuffer input, I typically define an \"initial\"
string rather than a \"default\", because an initial string
is easily and obviously editable.  It's a good point that 
the newer minibuffer history features get you much of the 
same functionality, but they're not terribly obvious (personally, 
I've only just realized that they were there, and I've been 
an emacs user for quite a long time).  The claim that 
defaults are better than initial values because they're less 
\"intrusive\" strikes me as a relatively abtruse issue, 
in comparsion.

It would probably be better if perlnow were a global minor-mode 
with a set of built-in keymappings, but for now I've decided to 
punt, and just instruct the user to add them to their global 
key map in their .emacs file.  \(Whenever I research this issue, 
my eyes begin to glaze over...\).

Similarly, rather than master the intricacies of texinfo, I'm 
copping out and entering documentation as variable docstrings 
such as this.  I picked up this idea from looking at IZ's 
cperl-mode, and I expect it appeals to me for the same reason 
it appeals to him: we're perl programmers, and we're used to 
\"pod\".

I've adopted the practice of inserting horizontal rules
between my function definitions (as suggested in a style
guide written by the tinytools folks), because this makes it
possible to use white space between chunks of code within
the defuns without confusing things.  My comment style remains 
strongly influenced by perl culture \(elisp people seem to think 
it's possible to write \"self-documenting\" code...\). 

Oh, and one last set of issues: for now I'm completely
ignoring the newer emacs features for menubars and
customization, because I don't know anything about them.  
I never use them.  I'm a \(menu-bar-mode -1\) kind-of guy.
Not to mention: \(scroll-bar-mode -1\) and \(tool-bar-mode -1\).")




;;;;##########################################################################
;;  User Options, Variables
;;;;##########################################################################


; TODO:
; on the following two, I'm currently using HOME environment variable for
; a default location, though it's expected this will be overriden with a
; .emacs setting.  Maybe it would be better to default to something else,
; like ~/bin and ~/lib, but in that case would have to make sure they
; exist and maybe create them otherwise. 
; Or see if they exist, and then use them, if not, silently fall back on HOME?

(defcustom perlnow-script-location (getenv "HOME")
  "This is the default location to stash new perl scripts.")
(setq perlnow-script-location (file-name-as-directory perlnow-script-location)) 

(defcustom perlnow-module-location (getenv "HOME")
  "This is the default location to stash new perl modules.")
(setq perlnow-module-location (file-name-as-directory perlnow-module-location))

(defcustom perlnow-h2xs-location perlnow-module-location 
  "This is the default location to do h2xs development of CPAN bound modules.")

(defcustom perlnow-executable-setting ?\110
  "The user-group-all permissions used to make a script executable.")

(defcustom perlnow-perl-script-template 
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pl.tpl")
"The template that new perl scripts will be created with.")
(put 'perlnow-perl-script-template 'risky-local-variable t)

(defcustom perlnow-perl-module-template 
  (substitute-in-file-name "$HOME/.templates/TEMPLATE.perlnow-pm.tpl")
"The template that new perl modules will be created with.")
(put 'perlnow-perl-module-template  'risky-local-variable t)

(defvar perlnow-perl-package-name nil
  "Used internally to pass the module name to the new module template.
Defines the PERL_MODULE_NAME expansion.")

(defvar perlnow-package-name-history nil
"The minibuffer history for perl modules accessed by this package.")

;;;----------------------------------------------------------
;; Defining additional "expansions" for use in template.el templates.
;; 
(defvar perlnow-documentation-template-expansions t
  "The perlnow template.el templates use some custom
expansions defined here in perlnow code.  A template.el
\"expansion\" is a place holder in the template that
gets replaced by something else when the template is
used.  For example, (>>>DATE<<<) will become the
current date.

The perlnow custom expansions: 

\(>>>PERL_MODULE_NAME<<<\)
becomes the perl module name \(in double-colon
separated form\) when used by \\[perlnow-module]
function.

\(>>>MINIMUM_PERL_VERSON<<<\)
The minimum perl version you usually support.  Gets used in 
the first line in a perl module, e.g. \"use 5.006;\". 
Used by \\[perlnow-module] to insert the value of 
`perlnow-minimum-perl-version'.

\(>>>TAB<<<\)
Experimental feature: should indent as though the tab
key had been hit.  I suspect that you need to insert
\(>>>TAB<<<\) *after* the line of code and not before.

\(>>>PNFS<<<\)
stands for \"PerlNow Filename Spaces\" it should 
always insert the same number of spaces as characters
in the name of the file.  This is a gross kludge
which can be used to get formatting to line up, for example:
   \(>>>FILE<<<\)              \(>>>AUTHOR<<<\)
   \(>>>PNFS<<<\)              \(>>>DATE<<<\)
Note the utility of having \"PNFS\" be four characters, 
the same length as \"FILE\".  Like I said: a gross kludge.

Some experimental, alternative gross kludges: 

\(>>>EMAIL_AT_45<<<\)
This moves to column 45 before inserting the user email address 
\(as understood by emacs, typically from a .emacs file setting\)

Note that this will obediently over-write anything else that might 
already be in that area.

\(>>>TIMESTAMP_AT_45<<<\)
This moves to column 45 before inserting the timestamp 
returned by current-time-string.
Note that this will obediently over-write anything else that might 
already be in that area.

See `template-expansion-alist' for the current list of 
defined expansions.")

; Now the actual definitions:

(setq template-expansion-alist 
      (cons 
      '("PERL_MODULE_NAME" (insert perlnow-perl-package-name) )
      template-expansion-alist))

(setq template-expansion-alist 
      (cons 
      '("PNFS" 
        (perlnow-insert-spaces-the-length-of-this-string (buffer-file-name)))
      template-expansion-alist))

(setq template-expansion-alist 
      (cons 
      '("TAB" (indent-according-to-mode) )
      template-expansion-alist))

(setq template-expansion-alist 
      (cons 
      '("EMAIL_AT_40" ((lambda ()
                         (move-to-column 40 t)      
                         (insert user-mail-address)
                       )))
      template-expansion-alist))

(setq template-expansion-alist 
      (cons 
      '("TIMESTAMP_AT_40" ((lambda ()
                         (move-to-column 40 t)      
                         (insert (current-time-string))
                       )))
      template-expansion-alist))


(defvar perlnow-minimum-perl-version "5.006"
  "The minimum perl version you are interested in supporting. 
This is used to define the template expansion of MINIMUM_PERL_VERSON.
Note that perl version numbers jumped from 5.006 to 5.7.0.  As of
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
;;; admonition in the emacs lisp ref.  
;;; (1) this makes the code a little simpler (I don't want to have 
;;; to remember to use make-local-variable in different places); 
;;; (2) I can't think of a case where the user would be annoyed at 
;;; me depriving them of this choice. 

(defvar perlnow-script-run-string nil 
   "The run string for perl scripts, used by \\[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \\[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(put 'perlnow-script-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-script-run-string)

(defvar perlnow-module-run-string nil 
   "The run string for perl modules, used by \\[perlnow-run]. 
Leave this set to nil unless you want to override the heuristics 
used by \\[perlnow-set-run-string] to determine the way to run 
the current script.  This is a buffer local variable, i.e. it 
may be set differently for different files.")
(put 'perlnow-module-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-module-run-string)

(defvar perlnow-run-string nil 
   "Tells \\[perlnow-run] how to run the code in a particular file buffer.
This is a buffer local variable which is set by \\[perlnow-script-run-string], 
and this should not typically be set by the user directly.  
See `perlnow-script-run-string' and `perlnow-module-run-string' instead.")
(put 'perlnow-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-run-string)

(defcustom perlnow-test-path (list "." "../t" "./t")
   "List of places to look for test scripts (*.t).
Typically these will look like paths specified as relative to 
the current directory via \".\" or  \"..\", though rather than 
the actual \"current\" location, they will be interpreted as 
relative to either the module root or the module location.")
(put 'perlnow-test-path  'risky-local-variable t)

(defcustom perlnow-simple-hash-bang-line "#!/usr/bin/perl -w"
  "A typical hash bang line for perl code.  
Used only by the somewhat deprecated \"simple\" functions: 
\\[perlnow-script-simple] \\[perlnow-perlify-this-buffer-simple]")


;;;==========================================================
;;; User Commands
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-run-check ()
  "Run a perl check on the current buffer.
This displays errors and warnings in another window, in the 
usual emacs style: After running this, you can skip to 
the location of the next problem with \\\[next-error]\n 
This command is like \\\[cperl-check-syntax] with one 
less prompt \(also, it does not require mode-compile.el\)."
  (interactive)
  (save-buffer)
  (setq compile-command (format "perl -cw \'%s\'" (buffer-file-name)))
  (message "compile-command: %s" compile-command)
  (compile compile-command) )
   
;;;----------------------------------------------------------
(defun perlnow-do-script (filename)
  "Quickly jump into development of a new perl script. 
Prompts the user for the FILENAME."
  (interactive 
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (require 'template) 
  (perlnow-create-with-template filename perlnow-perl-script-template)
  (perlnow-change-mode-to-executable))
   
   
;;;----------------------------------------------------------
(defun perlnow-script-using-this-module (script-name)
  "Quickly jump into a new script that uses the module code 
in the currently open buffer.  If the module is not in perl's 
search path \(@INC\), then an appropriate \"use lib\" statement 
will be added. \n
Note: if multiple packages exist in the file \\(and that's never 
really done\\) then this function will see the first package name."
  (interactive 
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (require 'template) 
  (let* ( (module-filename (buffer-file-name))
          (module-location (file-name-directory module-filename))
          (package-name (perlnow-get-package-name-from-module-buffer)) 
          (inc-spot (perlnow-get-inc-spot package-name module-location))
          ) 
    (unless package-name 
      (error "%s" "This file doesn't look like a perl module (no leading package line)."))

    (perlnow-do-script-from-module script-name package-name inc-spot)))

;;;----------------------------------------------------------
(defun perlnow-do-script-from-module (script-name package-name &optional inc-spot)
  "Does the work of creating a script from a module-buffer. 
Takes arguments SCRIPT-NAME PACKAGE-NAME INC-SPOT,
which are all explained in `perlnow-documentation-terminology'.
If INC-SPOT is nil, it skips adding the FindBin/use lib lines.
Used by the old \\[perlnow-script-using-this-module], and the 
newer \\[perlnow-script].  Always returns t, but someday 
it might return nil for failure."
;;; Skipping FindBin/use if inc-spot is nil is a hack to deal with 
;;; the case where we got the module name from a man page buffer, 
;;; and it's not so easy to say where the pm file is really installed.
;;; It's reasonably likely that it is though, and it's reasonably 
;;; likely that it's already in the @INC. 
;;; For now I just assume that it is.  
;;; Maybe later I'll institute some checking. (TODO?)

    ; We expect to use the new script will be used to run the code 
    ; in this module, so make the new script name the default runstring 
    ; for this module before we leave it.
    (setq perlnow-module-run-string (format "perl %s" script-name))

    (perlnow-sub-name-to-kill-ring)

    ; force a two window display, existing module and new script
    (delete-other-windows) 
    (split-window-vertically)
    (other-window 1)

    (perlnow-create-with-template script-name perlnow-perl-script-template)

    (unless (eq inc-spot nil) ; without inc-spot, don't mess with FindBin/lib
      (progn
        ; ensure the module can be found by the script if needed, insert "use lib" line 
        (unless (perlnow-inc-spot-in-INC-p inc-spot)
          (let ((relative-path
                 (file-relative-name inc-spot (file-name-directory script-name))
                 ))
            (insert "use FindBin qw\($Bin\);\n")
            (insert "use lib \(\"$Bin/")
            (insert relative-path)
            (insert "\");\n"))))

      ; insert the "use Some::Module;" line
      (insert (format "use %s;" package-name)) ;;; and maybe a qw() list? 
      (insert "\n"))
    t)
   
;;;----------------------------------------------------------
(defun perlnow-sub-name-to-kill-ring ()
  "Pushes the name of the current perl sub on to the kill-ring.
This is intended to be run inside an open buffer of perl code. 
It tries to find the name of the current perl sub \(the one that 
the cursor is inside of\) and pushes it onto the kill-ring, ready 
to be yanked later.  Returns nil on failure, sub name on success. 
Used by \\[perlnow-script-using-this-module]."
  (interactive) ; DEBUG only DELETE
  (let (return) 
  (save-excursion
    ; in case the cursor is *on top* of the keyword "sub", go forward a little.
    (forward-word 1) 
    (forward-char)
    (setq return
          (catch 'HELL
            (unless (re-search-backward "^[ \t]*sub " nil t)
              (throw 'HELL nil))
            ; jump to start of name
            (forward-word 1) 
            (forward-char)
            (let ((beg (point)))
              (unless (re-search-forward "[ \\\\(\\{]" nil t)
                (throw 'HELL nil))
              (backward-word 1)
              (forward-word 1) 
              (copy-region-as-kill beg (point))
              (setq return 
                    (buffer-substring-no-properties beg (point)))
              ))))
  return))


;;;----------------------------------------------------------
(defun perlnow-script (script-name)
  "General purpose command to quickly jump into coding a perl script. 
This prompts the user for the new SCRIPT-NAME, then 
looks at the current buffer and tries to guess what \"use\" lines
you might want to start coding with.  If it's a perl module, or a man page 
documenting a perl module, it will give you a \"use\" line to include 
that module.  If the module is not in perl's @INC array, it will also 
insert the appropriate \"FindBin\" & \"use lib\" lines so that the script 
can find the module. If none of that applies, you just get the usual 
perl script buffer.\n
If this works well, it obviates \\[perlnow-do-script] and 
\\[perlnow-script-using-this-module].  If it doesn't they're still there."
  (interactive
   (perlnow-prompt-user-for-file-to-create 
    "Name for the new perl script? " perlnow-script-location))
  (require 'template) 
  (let ( package-name) 
    (cond 
     ((setq package-name (perlnow-get-package-name-from-module-buffer))
       (let* ( (module-filename (buffer-file-name))
               (module-location (file-name-directory module-filename))
               (inc-spot (perlnow-get-inc-spot package-name module-location)) )
        (setq perlnow-perl-package-name package-name) ; global used to pass value into template
        (perlnow-do-script-from-module script-name package-name inc-spot) ))

      ((setq package-name (perlnow-get-package-name-from-man))
        (setq perlnow-perl-package-name package-name) ; global used to pass value into template
        (perlnow-do-script-from-module script-name package-name))
      (t ; no package name found, so we're working with a script
         ; (someday, might use perlnow-script-p)
       (perlnow-do-script script-name)))))
;;;
;;;      (setq module-filename (perlnow-module-found-in-INC package-name))
;;;         ; given colon-ized, returns first pm found, or nil if none
;;;         ; (no sense in this if you're in module code, only for mod man case)
;;;         ; (and if we *can* find this, then there's no need for FindBin/use lib, so...?)
;;;    Someday: maybe report problem if not found, e.g.
;;;    Insert comment in code file, by use lib: # Currently not found in @INC. Installed correctly?

;;;----------------------------------------------------------
(defun perlnow-module-found-in-INC (package-name) 
  "Given a perl PACKAGE-NAME \(in double-colon separated form\) 
return the first module file location found in perl's @INC 
array, or nil if it is not found."
;;; TODO That "|||" shit is cheesy.  Use "\t" or something, huh?
;  (interactive "sGimme:") ; DEBUG only DELETE
  (let* (  full return
           (module-file-tail 
            (concat (replace-regexp-in-string "::" "/" package-name) ".pm"))
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
(defun perlnow-module-two-questions (inc-spot package-name) 
  "Quickly jump into development of a new perl module. 
This is an older, but simpler form that asks the user two 
questions to get the INC-SPOT and the PACKAGE-NAME.  The 
newer \\[perlnow-module\] uses a hybrid form to get that 
information in a single question.  This function is still provided 
for people who don't don't agree that that's more convenient."
  (interactive 
   ; Because default-directory is the default location for (interactive "D"),
   ; I'm doing the interactive call in two stages: change 
   ; default-directory momentarily, then restore it. Uses dynamic scoping via "let".
   ; (It's more like perl's "local" than perl's "my".)
   (let ((default-directory perlnow-module-location))
     (call-interactively 'perlnow-prompt-for-module-to-create)))
  (require 'template) 
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module inc-spot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))
   

;;;----------------------------------------------------------
(defun perlnow-prompt-for-module-to-create (where what) 
  "Internally used by \\[perlnow-module-two-questions\] to ask the two questions. 
Asks for the WHERE, i.e. the \"module root\" location, and the WHAT, the name 
of the perl module to create there.  Checks to see if one exists already, 
and if so, asks for another name.  The location defaults to the current 
`default-directory'.  Returns a two element list, location and package-name.\n
Note: This is all used only by the mildly deprecated \\[perlnow-module-two-questions\]."
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
  "Prompt the user for a new string run string for the current buffer.
This sets the global variable `perlnow-run-string' that \\[perlnow-run]
will use to run the code in future in the current buffer. 
Frequently, the user will prefer to use \\[perlnow-run] and let it 
run this command if need be; however using this command directly is 
necessary to change the run command string later. \n
From within a program, you'd probably be better off setting the variables 
directly, see `perlnow-script-run-string' and `perlnow-module-run-string'.\n

This function uses \\\[perlnow-module-code-p] to see if the code looks like a
module (i.e. does it have a package line), otherwise it 
assumes it's a perl script."
;; And if it's not perl at all, that's your problem: the obvious
;; tests for perl code, like looking for the hash-bang,
;; aren't reliable (perl scripts need not have a hash-bang
;; line: e.g. *.t files, windows perl...).
  (interactive)
   (cond
   ((perlnow-module-code-p)
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
         (setq perlnow-module-run-string 
               (perlnow-guess-script-run-string))
         ))
     ; ask user how to run this script (use as default next time)
     (setq perlnow-script-run-string 
           (read-from-minibuffer 
            "Set the run string for this script: " 
            perlnow-script-run-string))
     ; tell perlnow-run to do it that way
     (setq perlnow-run-string perlnow-script-run-string))))

;;;----------------------------------------------------------
(defun perlnow-run (runstring) 
  "Run the perl code in this file buffer.
This uses an interactively set RUNSTRING determined from 
`perlnow-run-string' which may have been set by using 
\\[perlnow-set-run-string]. If `perlnow-run-string' is nil, 
\\[perlnow-set-run-string] is called automatically.\n
The run string can always be changed later by running 
\\[perlnow-set-run-string] manually."
  (interactive
   (let (interactive-return)
   (if (eq perlnow-run-string nil)
       (setq interactive-return (perlnow-set-run-string))
     (setq interactive-return perlnow-run-string))
   (list interactive-return)
   ))
  (compile runstring))

;;;----------------------------------------------------------

(defun perlnow-perldb (runstring) 
  "Runs the perl debugger on the code in this file buffer.
This uses an interactively set RUNSTRING determined from 
`perlnow-run-string' which may have been set by using 
\\[perlnow-set-run-string]. If `perlnow-run-string' is nil, 
\\[perlnow-set-run-string] is called automatically. 
It can always be changed later by running \\[perlnow-set-run-string] 
manually. \n
There's a major advantage that this command has over running 
\\[perldb] directly: you can have different `perlnow-run-string'
settings for different file buffers \(i.e. it is a buffer local 
variable\).  Unfortunately \(as of this writing\) \\[perldb] 
used directly always re-uses it's previous run-string as a 
default, and that's guaranteed to be wrong if you've switched 
to a different file."
  (interactive
   (let (interactive-return)
   (if (eq perlnow-run-string nil)
       (setq interactive-return (perlnow-set-run-string))
     (setq interactive-return perlnow-run-string))
   (list interactive-return)
   ))
  (perldb runstring))

;;;----------------------------------------------------------
(defun perlnow-h2xs (h2xs-location package-name) 
  "To quickly jump into development of a new perl CPAN module.
Asks two questions, prompting for the H2XS-LOCATION  \(the place where 
h2xs will create the \"staging area\"\) and the PACKAGE-NAME \(in perl's 
double-colon separated package name form\)."
  (interactive 
; Because default-directory is the default location for (interactive "D"),
; I'm doing the interactive call in stages: this way can change 
; default-directory momentarily, then restore it. Uses the dynamic scoping 
; of elisp's "let" (which is more like perl's "local" than perl's "my").
  (let ((default-directory perlnow-h2xs-location))
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
                (concat "-n" package-name)
                (concat "-b" 
                        (perlnow-perlversion-old-to-new perlnow-minimum-perl-version)))

  (find-file 
   (perlnow-full-path-to-h2xs-module h2xs-location package-name))
  (search-forward "# Preloaded methods go here.")
  (forward-line 1)   ;alternate: (next-line 1)

  ; Also open the *.t file (note, this presumes the modern naming style)
  ; would break for older *.t style names, e.g. 1.t. TODO  Generalize?
  (other-window 1)
  (find-file
   (perlnow-full-path-to-h2xs-test-file h2xs-location package-name))
  (search-forward "BEGIN { plan tests => 1")
  (other-window 1)
  ))


;;;----------------------------------------------------------
(defun perlnow-prompt-for-h2xs (where what) 
  "For Internal use only: ask the two questions for \\[perlnow-h2xs].
The WHERE is location to put the h2xs structure and the WHAT is 
the name of the perl module to create.  Checks to see if one exists 
already, and if so, asks for another name (by doing yet another
\\[call-interactively] of another function).  The location
defaults to the current `default-directory'.  Returns a two
element list, location and package-name."
  (interactive "DLocation for new h2xs structure? \nsName of new module \(e.g. New::Module\)? ")

  (let ( location-in-staging-area        
         )
  (setq location-in-staging-area 
        (concat (perlnow-fixdir where)
                (mapconcat 'identity (split-string what "::") "-")))

  (while (file-exists-p location-in-staging-area)  ;;; really, directory exists
    (setq where-and-what  ; (h2xs-location package-name)
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
  "For internal use only: the \"ask again\" for \\[perlnow-h2xs\].
If the user enters an existing h2xs module name in 
\\[perlnow-prompt-for-h2xs], it will do another chained \\[call-interactively]
to this function to ask again for WHERE and WHAT with a slightly 
different message.  Returns a two element list, location and package-name."
  (interactive "DThat exists already! Location for new h2xs structure? \nsName of new module \(e.g. New::Module\)? ")
  (list where what))


;;;==========================================================
;;; Internally used functions 
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-insert-spaces-the-length-of-this-string (string) 
  "Inserts as many spaces as characters in the given string.
Used by the template.el expansion PNFS."
  (insert 
   (make-string (length 
                 (file-name-nondirectory string)
                 ) ?\ )))

;;;----------------------------------------------------------
(defun perlnow-full-path-to-module (inc-spot package-name)
  "Piece together a INC-SPOT and a PACKAGE-NAME into a full file name.
Given \"/home/doom/lib\" and the perl-style \"Text::Gibberish\" would 
yield /home/doom/lib/Text/Gibberish.pm or in other words, the 
filesys path."
  (let ((filename 
         (concat 
          (mapconcat 'identity (split-string package-name "::") "/")
          ".pm")))
  (setq inc-spot (file-name-as-directory inc-spot)) 
  (concat  inc-spot filename)))

;;;----------------------------------------------------------
(defun perlnow-make-sure-file-exists ()
  "Forcibly save the current buffer to it's associated file.
This is to make sure that the file actually exists."
  (set-buffer-modified-p t)
  (save-buffer))

;;;----------------------------------------------------------
(defun perlnow-change-mode-to-executable ()
  "Make the file associated with the current buffer executable."
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
  "Ask for the name of the file to create. 
Check to see if one exists already, and if so, ask for another name.  
Asks the question ASK-MESS, and defaults to the using the location 
DEFAULT-LOCATION.  Returns a list of a single string, full file name 
with path."
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
(defun perlnow-create-with-template (filename template)
  "Create a new file with a template.el template.
Given FILENAME and TEMPLATE this does the actual creation of
the file and associated buffer using the template.  As a 
side-effect, it sets the global `template-file' here."
; Because of a bug in template.el, when using template-new-file 
; non-interactively, we must set the global "template-file" here:
  (setq template-file (template-split-filename filename)) 
  (template-new-file filename template)
  (write-file filename))

;;;----------------------------------------------------------
(defun perlnow-nix-script-p ()
  "Determine if the buffer looks like a 'nix style executable script.
Looks for the hash-bang line at the top."
  (save-excursion 
  (let ( (hash-bang-line-pat "^[ \t]*#!") )
    (goto-char (point-min))
    (looking-at hash-bang-line-pat) 
    )))

;;;----------------------------------------------------------
(defun perlnow-script-p ()
  "Determine if the buffer looks like a perl script.
Looks for the hash-bang line at the top.  Note: this is probably not 
a reliable test, since some perl scripts will not have a hash-bang line, 
e.g. test files \(*.t\) or scripts on non-unix-like systems."
  (save-excursion 
  (let ( (hash-bang-line-pat "^[ \t]*#!.*perl\\b") ) ; note, presumes an explicit "perl"
    (goto-char (point-min))
    (looking-at hash-bang-line-pat))))

;;;----------------------------------------------------------
(defun perlnow-module-code-p ()
  "Determine if the buffer looks like a perl module. 
This looks for the package line near the top."
  (save-excursion 
  (let ( (package-line-pat "^[ \t]*package\\b") 
         (comment-line-pat "^[ \t]*$\\|^[ \t]*#") )
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (looking-at package-line-pat) )))

;;;----------------------------------------------------------
(defun perlnow-get-package-name-from-module-buffer () 
  "Get the module name from the package line.
This will be in perl's double colon separated form, or it will
return nil if none is found."
  (save-excursion 
  (let ((package-line-pat "^[ \t]*package[ \t]*\\(.*\\)[ \t;]") ;; captures "Module::Name"
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
         return)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat) 
        (setq return (match-string 1))
      (setq return nil))
    (set-text-properties 0 (length return) nil return) ; remove all text properties
    return)))

;;;----------------------------------------------------------
(defun perlnow-get-package-name () 
  "Return the module name  \(in perl's double colon separated form\)
from either a module buffer or a Man page showing the perldoc for it, 
or nil if none is found.  Currently, not used: typically want 
to *know* if it came from a code buffer or a man page, this throws 
away that info."
  (let (return)
    (cond 
     ((setq return (perlnow-get-package-name-from-module-buffer))
       )
     ((setq return (perlnow-get-package-name-from-man))
       )
     (t
      (setq return nil)
      ))
    return))

;;;----------------------------------------------------------
(defun perlnow-get-package-name-from-man ()
  "Return the module name from a man page buffer displaying the perldoc.
If not a man page buffer, returns nil.  It tries several methods of 
scraping the module name from the man page buffer, and returns 
it's best guess."
;  (interactive)                         ; DEBUG only DELETE
  (save-excursion
    (let ( return buffer-name-string candidate-list 
           candidate-1 candidate-2 candidate-3 
           (buffer-name-string (buffer-name))
           )
      (cond  
       ((string-match "\\*Man \\(.*\\)\\*$" (buffer-name))
          (setq candidate-1 (match-string 1 buffer-name-string))
          (setq candidate-list (cons candidate-1 candidate-list))

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
  "Pick the most commonly occuring string from a list of strings.
The list should be given as the argument CANDIDATE-LIST, 
the return value will be the string itself.  In the event of a tie
this favors the earlier occurance in the list."
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
  "Get an absolute path to the location one above the given location."
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
  "Fixes the DIR.
This does the many cool and groovy elispy things that are a
good idea for conditioning directory paths for portability and 
robustness.  I don't always know when these things are needed, 
but now that I've got them all in this one, easy to use function, 
I will just use it all the goddamn time, and all of my problems 
will be a thing of the far distant galactic past."
  (let ((return
  (convert-standard-filename
   (file-name-as-directory
    (expand-file-name dir)))))
    return))

;;;----------------------------------------------------------
(defun perlnow-expand-dots-relative-to (dot_means given_path)
  "Using the dot definition DOT_MEANS, expand the GIVEN_PATH.
Given a directory path that leads with  \".\" or \"..\" 
expand to an absolute path using the given DOT_MEANS as 
the value for \".\".  Note: currently this is limited to 
*leading* dot expressions, and can not handle weirder stuff 
like: \"/home/doom/tmp/../bin\"."
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
  "Return the lowest level name from a given directory path.
For example, given DIR: \"/usr/lib/perl/\" this returns: \"perl\"."
  (let* ( (levels (split-string dir "/"))
          (return (nth (- (length levels) 1) levels)) )
    return))

;;;----------------------------------------------------------
(defun perlnow-guess-module-run-string ()
  "Return a good guess for an appropriate `perlnow-module-run-string'. 
First looks for the Makefile \(or Makefile.PL\) of an h2xs set-up.
Failing that it looks for a nearby test file of an appropriate name.
For example if the module were named New::Module, the test file 
could be New-Module.t or Module.t.  It searches the paths in 
`perlnow-test-path', which uses a familiar dot notation \(\".\" \"..\"\) 
to specify them relative to \"here\", where \"here\" means either 
the module-file-location or the inc-spot \(both interpretations 
are checked\). \n
If this seems too complex, that's because it is, but it does make 
it convenient to use this with a number of reasonable organizational 
schemes for your test files: `perlnow-tutorial-test-file-strategies'."

;;; TODO
;;; o  Will also at some point want a "perlnow-edit-test-file-for-this-module".
;;; Maybe this code should be revamped (again, sigh): need routine that returns test file name?
;;; o  Still another want would be "perlnow-create-test-file-for-module" which would need 
;;; to read policy from somewhere, to know where to put it and what to call it. 
;;; My pick for policy: if inside of an h2xs structure, put in the appropriate "t", 
;;; otherwise create a local t for it, but use the full hyphenized module name as 
;;; base-name (to make it easy to move around without confusion). 
;;; How to specify policy?  Three pieces of info: 
;;;   1 - A dot form, e.g. "./t"
;;;   2 - a definition of dot e.g. module-file-location
;;;   3 - name style, e.g. hyphenized

  (unless (perlnow-module-code-p) 
    (error "This buffer does not look like a perl module (no \"package\" line)"))
  (let* ( (package-name (perlnow-get-package-name-from-module-buffer))
          (module-file-location 
            (file-name-directory (buffer-file-name)))
          (inc-spot 
            (perlnow-get-inc-spot package-name module-file-location ))
          (hyphenized-package-name 
            (mapconcat 'identity (split-string package-name "::") "-"))
          (module-file-basename 
            (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

          ;;; TODO - Consider exposing a this list to users in some form,
          ;;;        via a defvar or something
          ; This is a listing of possible names for the test file:
          (test-file-check-list (list (concat hyphenized-package-name ".t")
                                      (concat module-file-basename ".t")
                                      ))

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
            (setq staging-area-candidate (perlnow-one-up inc-spot))
            (setq staging-area-candidate-name 
                  (perlnow-lowest-level-directory-name staging-area-candidate))
            (cond
             ((string= staging-area-candidate-name hyphenized-package-name)
              (setq staging-area (perlnow-fixdir staging-area-candidate)) 
              (cond 
               ((file-regular-p (concat staging-area "Makefile"))
                (setq water "make test")
                (throw 'COLD water))
               ((file-regular-p (concat staging-area "Makefile.PL"))
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
                    (perlnow-expand-dots-relative-to inc-spot testloc-dotform))
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
;                      (setq fish (format "perl %s" testfile))
                      (setq fish
                            (format "perl \"-MExtUtils::Command::MM\" -e \"test_harness(1, %s)\"" testfile)
                      (throw 'COLD fish)))))))
    return)))

;;;----------------------------------------------------------
(defun perlnow-guess-script-run-string ()
  "Returns a good guess for `perlnow-script-run-string'."
;;# TODO DONTFORGET
;;# check for hash bang:
;;#   Pass through at least *some* elements of hash-bang, e.g. -T (why not all?)
;;# check buffer-file-name, if extension .t
;;#   If a .t is it in an h2xs structure?  "make test"
;;#   Otherwise: (format "perl \"-MExtUtils::Command::MM\" -e \"test_harness(1, %s)\"" testfile)
  (setq perlnow-script-run-string 
        (format "perl %s" (buffer-file-name)))
)

;;;----------------------------------------------------------
(defun perlnow-get-inc-spot (package-name module-location)
  "Determine the module root, the place where the package namespace begins.
Given the PACKAGE-NAME \(e.g. \"New::Module\"\), 
and the MODULE-LOCATION \(as an absolute path to the \".pm\" file, 
e.g. \"/home/doom/perldev/Punk/Skunk/New/Module.pm\"\), 
this returns the module root, \(which in this example is:
\"/home/doom/perldev/Punk/Skunk/\"\) Returns nil if module-location is nil."
;; Example: 
;;  /home/doom/perldev/Punk/Skunk/New/Module.pm 
;;  /home/doom/perldev/Punk/Skunk/New/              => number of levels:  7
;;                                New::Module       => double-colon-count: 1
;;  /home/doom/perldev/Punk/Skunk/                  The desired inc-spot
;;
  (let (double-colon-count  ; count of '::' separators
        file-levels-list    ; list of directories in the path
        inc-spot)        ; 
    (cond ((eq module-location nil) 
           (setq inc-spot nil))
          (t 
           (setq double-colon-count (- (length (split-string package-name "::")) 1))
           (setq file-levels-list (split-string module-location "/"))
           (setq inc-spot (mapconcat 'identity 
                                     (butlast file-levels-list double-colon-count)
                                     "/"))
           (setq inc-spot (concat "/" inc-spot)) ; kludge, must prepend a "/" 
                                                 ; (thus code breaks if not given full-path)
           ))
    inc-spot))


;;;----------------------------------------------------------
(defun perlnow-perlversion-old-to-new (old-version)
  "Convert old form of perl version into the new form.
For example, an OLD-VERSION might be 5.006 for which the new is 5.6.0 
which is more suitable for use as the -b parameter of h2xs."
  (let ( (old-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)$")
         major
         mantissa 
         minor1)
  (if (string-match old-version-pat old-version)
      (progn 
        (setq major (match-string 1 old-version)) 
        (setq mantissa (match-string 2 old-version)))
    (error "Does not look like an old-style perl version: %s" old-version))
  (setq minor1 (substring mantissa 2))
  (concat major "." minor1 "." "0")))


;;;----------------------------------------------------------
(defun perlnow-full-path-to-h2xs-module (h2xs-location package-name)
  "Get the full path to a module created by h2xs.  
E.g. if the H2XS-LOCATION were \"/usr/local/perldev\" and the PACKAGE-NAME
were \"New::Module\", this should return: 
\"/usr/local/perldev/New-Module/lib/New/Module.pm\""
  (let ((module-filename 
         (concat 
          (file-name-as-directory h2xs-location)
          (mapconcat 'identity (split-string package-name "::") "-")
          "/lib/"
          (mapconcat 'identity (split-string package-name "::") "/")
          ".pm")))
    module-filename))

;;;----------------------------------------------------------
(defun perlnow-full-path-to-h2xs-test-file (h2xs-location package-name)
  "Get the full path to a the test file for a module created by h2xs.  
E.g. if the H2XS-LOCATION were \"/usr/local/perldev\" and the 
PACKAGE-NAME  were \"New::Module\", it should return: 
\"/usr/local/perldev/New-Module/t/New-Module.t\""
  (let* ( return 
         (module-test-location
          (concat 
           (file-name-as-directory h2xs-location)
           (mapconcat 'identity (split-string package-name "::") "-")
           "/t/"))
         (module-filename
          (concat 
           module-test-location
           (mapconcat 'identity (split-string package-name "::") "-")
           ".t")))
    (cond ((file-exists-p module-filename) 
           (setq return module-filename))
          ((file-directory-p module-test-location) 
           (setq return module-test-location))
           (t 
           (error "Can't find h2xs test file or test location")
           ))
    module-filename))

;;;----------------------------------------------------------
(defun perlnow-blank-out-display-buffer (buffer)
  "Clear out a temporary display BUFFER.
Erase the contents of a buffer, though only if it matches 
the convention for temporary display buffers, i.e. it has
a name beginning with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument BUFFER can be a string or 
a buffer object.  This can work on a read-only buffer."

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
(defun perlnow-inc-spot-in-INC-p (&optional inc-spot)
  "Determine if the INC-SPOT has been included in perl's @INC search path.
If not given a INC-SPOT, it defaults to using the module root of the 
current file buffer."
;;; The issue: 
;;; If the module root \(i.e. the beginning of the package name space 
;;; for a given module\) is in @INC already then we won't need to 
;;; do a \"use lib\" to access it. 
;;; Note: 
;;; Just checking getenv("PERL5LIB") would be close, but 
;;; using @INC as reported by perl seems more solid, so that's 
;;; what we do here.
;;; TODO That "|||" shit is cheesy.  Use "\t" or something, huh?
  (unless inc-spot
    (setq inc-spot 
          (perlnow-get-inc-spot 
           (perlnow-get-package-name-from-module-buffer)
           (file-name-directory (buffer-file-name)))))

    (let* (
      (perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_|||\"}'" ))
      (inc-path-list (split-string perl-inc "|||"))
      return )
      (setq return 
            (catch 'UP
              (dolist (path inc-path-list)
                (if (string= path inc-spot)
                    (throw 'UP t)))))
      return))
;;; TODO
;;; Consider loading a lisp structure with @INC once early on, 
;;; so we won't need to do the above over and over... 

;;;==========================================================
;;; The following block of code is used by perlnow-module:
;;; perlnow-prompt-for-new-module-in-one-step and relatives 
;;; are used to read in perlmodule path and names in one step
;;; (A variant of the old perlnow-prompt-for-module-to-create.)
;;;
;;; Note: instead of completing-read this uses read-from-minibuffer 
;;; with a customized keymap that totally transforms it's behavior.
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
   "Keymap for reading a perl module name via the minibuffer.")
(put 'perlnow-read-minibuffer-map  'risky-local-variable t)
;;; TODO
;;; Look at minibuffer-local-map for hints on how to set up menu-bar: 
;;;     (define-key map [next] 'next-history-element)
;;;     (define-key map [prior] 'previous-history-element)


;;;----------------------------------------------------------
(defun perlnow-module (inc-spot package-name) 
  "Quickly jump into development of a new perl module.
In interactive use, gets the path INC-SPOT and PACKAGE-NAME 
with a single question, asking for an answer in a hybrid form 
like so:
   /home/hacker/perldev/lib/New::Module
This uses the file-system separator  \"/\" for the INC-SPOT 
location and then the perl package name-space separator \"::\" 
for the package-name.  Autocompletion works in a way very similar
to the usual emacs input methods for file names and paths, 
even after switching to the \"::\" separators, though after 
the string is input the transition from slash to double-colon 
is used to determine where perl's package namespace begins. \n
The \".pm\" extension is assumed and need not be entered. \n
If the module exists already, the user is asked for another name. \n
The location for the new module defaults to the global 
`perlnow-module-location'. The default location is used as the initial 
contents of the minibuffer, so that it may be edited at time of module 
creation."
;;; Formerly named: perlnow-prompt-for-new-module-in-one-step

  (interactive 
   (let ((initial perlnow-module-location)
         (keymap perlnow-read-minibuffer-map)   ; Note: the keymap is key. Totally transforms read-from-minibuffer.
         (history 'perlnow-package-name-history) 
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
           (perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name result))
     return))
  (require 'template) 
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module inc-spot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))


;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-complete ()
  "Does automatic completion of up to an entire directory or file name.  
Used in reading in path and name of a perl module \(which
need not exist already, though a portion of the file system
path for it may exist, and autocompletion should be
available for the parts that do exist\).  Valid name
separators are \(\"/\" or \"::\"\).\n 
This makes no attempt at a more aggressive completion past 
a file-system boundary."
;;; codename: new tabby
  (interactive)
  (let ((restrict-to-word-completion nil))
        (perlnow-read-minibuffer-workhorse restrict-to-word-completion)
    ))

;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-complete-word ()
  "Does automatic completion only up to the end of the next \"word\".
As opposed to an entire directory or file name as 
\\[perlnow-read-minibuffer-complete\] does. 
Used in reading in the name of a perl module name \(which need not 
exist already\), where valid name separators are \(\"/\" or \"::\"\)."
;; codename: new spacey
  (interactive)
  (let ((restrict-to-word-completion t))
    (perlnow-read-minibuffer-workhorse restrict-to-word-completion)
  ))

;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-workhorse (restrict-to-word-completion)
  "Does the actual work of auto-completion when reading a perl module name.
This is for reading a module path and name in hybrid form, ala 
\\[perlnow-module\].  This perl module need not exist already.  
This hybrid form has valid name separators: \(\"/\" or \"::\"\).  
Switching to double-colon form is the indicator that you're now in the 
perl package name space. 
Takes a single logical argument RESTRICT-TO-WORD-COMPLETION 
that controls whether whole name or single word completion will be used.
This switch is the sole difference between \\[perlnow-read-minibuffer-complete\] 
and \\[perlnow-read-minibuffer-complete-word\]."
;; codename: workhorse
;;;
;;; TODO 
;;; I'm hardcoding "/" as the "file system" separator 
;;; which means that this code will need to be fixed to get it 
;;; to work on a non unix-like system.
  (let ( ; empty declarations:
         raw_string candidate-alist suggested-completion field-start word-separator 
         two-pieces-list perlish-path fragment fragment-pat file-system-path
         lastchar returned new-portion new-portion-first-word result new-mini
          ; definitions
          (end-of-prompt-pat ": ")
          (pm-extension-pat "\\.pm$") )

    (setq raw_string (buffer-string))
    (string-match end-of-prompt-pat raw_string)
    (setq field-start (match-end 0)) ; also used later to blank minibuffer
    (setq minibuffer-string (substring raw_string field-start))


    ; No single trailing colons allowed: silently double them up
;;;    (if (string-match "[^:]:$" minibuffer-string)
;;;        (setq minibuffer-string (concat minibuffer-string ":")))

    (if (string-match "[^:]:$" minibuffer-string)
        (setq new-mini (concat minibuffer-string ":"))
      (progn ; else, do usual processing

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

;;; (unless (file-directory-p file-system-path)

    (setq candidate-alist (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
    (setq returned (try-completion fragment candidate-alist))
  
    ; must convert logical values of "returned" into appropriate strings 
    (cond ((eq returned nil)  
           (setq suggested-completion fragment))
          ((eq returned t) ; a precise match that is not a *.pm file is a directory: add separator
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
  
    ; peel off existing fragment from suggested-completion, what remains is the new-portion
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
   )) ; end if/else, close of "usual processing" 
  
    (delete-region (+ 1 field-start) (point-max))
    (insert new-mini)

;;;       )
    ))


;;;----------------------------------------------------------
(defun perlnow-read-minibuffer-completion-help ()
   "Show the available completions when reading in path & name of a module.
Most likely this will be called by \\\[perlnow-read-minibuffer-complete-word] 
and \\\[perlnow-read-minibuffer-complete] \(at least indirectly, through 
\\\[perlnow-read-minibuffer-workhorse])\), though it's also expected to 
be bound to the \"?\" key during the minibuffer read."
;;; codename: huh
  (interactive)
  (let* (
;;;         (raw_string (buffer-string))
         (raw_string (buffer-substring-no-properties (point-min) (point-max)))
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
         match-alist
         )
;    (unless (file-directory-p file-system-path)
      (setq match-alist (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
      (setq match-alist (perlnow-remove-pm-extensions-from-alist match-alist))
;      )
   (with-output-to-temp-buffer "*Completions*"
     (display-completion-list
      (all-completions fragment match-alist)
      ))
   ))

;;;----------------------------------------------------------
(defun perlnow-remove-pm-extensions-from-alist (alist)
  "Remove the pm extension from the names in the ALIST of file names and values.
Currently this throws away the numeric value and re-numbers the names in the 
alist in order."
; Does that behavior matter one way or another? 
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
  "Generate directory listing alist relevant to perl module creation.
Get a directory listing from the given FILE-SYSTEM-PATH, and return
an alist of the file and directory names that match certain criteria:
All the names must match the given PATTERN \(expected
to be of the form \"^leading_fragment\"\).  Further, the filenames 
are restricted to being perl module names \(ending in \"*.pm\"\) 
which also pass the \\[perlnow-interesting-file-name-p] test 
\(though that is probably redundant\). \n 
These are simple file names that do not include the path, 
and the values associated with them in the returned alist 
are sequential integers."
;;; For extra credit how about stripping the .pm on the file names?
;;; Nope: I can't do that, it messes up "workhorse" as written. 
   (let* ( 
          match-alist
          ; some directory-files arguments:
          (directory-full-name nil)
          (directory-nosort nil)
          (file-list 
            (directory-files file-system-path directory-full-name pattern directory-nosort))
          (i 1)  ; counter to build alist with numeric value
          )
     (dolist (file file-list)
       (if (perlnow-interesting-file-name-p file)
           (cond ((file-directory-p (concat file-system-path file))
                   (setq match-alist (cons (cons file i) match-alist))
                   (setq i (+ i 1)))
                 ((string-match "\\.pm$" file)
                   (setq match-alist (cons (cons file i) match-alist))
                   (setq i (+ i 1))))))
  ; Reverse the order of the match-alist to get values counting up starting from 1
  (setq match-alist (reverse match-alist))  ;; maybe this isn't needed, but cargo cult programming is fun
  ))


;;;----------------------------------------------------------
(defun perlnow-list-directory-as-alist (file-system-path pattern)
  "Generate a directory-only alist from the given FILE-SYSTEM-PATH.
Returns an alist of the file names that match the given PATTERN, *and*
which also pass the \\[perlnow-interesting-file-name-p]
test.  These are simple names not including the path, and
the values associated with them in the alist are sequential numbers
This is like \\[perlnow-list-directories-and-modules-as-alist]
\(which is more important\), but it does not include module names, 
it only lists directories."
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
(defun perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name (string)
  "Split the hybrid form of a module path into the two components.
Input STRING is expected to be a hybrid file system
path using slashes for the module root name space, and
double colons for the package name space inside of that.
This is split into two pieces, the module root 
and module name, which are returned as a two-element list."
  (let* ( (pattern 
            (concat 
             "^\\(.*\\)"       ; ^(.*)    - stuff at start becomes the mod root
             "/"               ; /        - the right-most slash, because: 
             "\\([^/]*\\)"     ; ([^/]*)  - mod name: everything that is not a slash up to  --
             "\\(\\.pm\\)*$"   ; (\.pm)*$ - the end (or an optional .pm extension)
             ))
           inc-spot 
           package-name
          )
         (cond ((string-match pattern string)
                (setq inc-spot (match-string 1 string))
                (setq package-name (match-string 2 string)) ) ; note: does not include any .pm
               (t
                (message "match failed: could not separate into module root and name.") )) 
         (list inc-spot package-name) ))


;;;----------------------------------------------------------
(defun perlnow-interesting-file-name-p (string)
  "Should given file or directory name be regarded as interesting?
Takes a bare filename (sans path) as the STRING 
argument and returns t if it doesn't match the list of
uninteresting filenames patterns, otherwise nil."
;;; TODO
;;; Shouldn't silently use completion-ignored-extensions.
;;; Break it out as a defvar
;;; "perlnow-interesting-file-name-pat" or something.  
;;; Let the user define what's interesting.
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
  "Split a file system path into directory and trailing name fragment.
Allows for the use of perl's double-colon package
name separators in addition to the usual unix-like slash
character.\n
Simple example: given the STRING \"/home/doom/lib/Stri\" should return 
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
;; The "simple" functions.  Older code that doesn't use template.el.
;;;==========================================================

;;;----------------------------------------------------------
(defun perlnow-script-simple ()
  "Quickly jump into development of a new perl script. 
This is a simple, though inflexible form of \\[perlnow-script].
One advantage: it does not require the template.el package."
;;; formerly: perlutil-perlnow
  (interactive)  
  ; ask the user the name of the script to create
  ; check to see if one exists already, and if so, ask for another name 
  (let ( (perlutil-ask-mess "Name for the new perl script? " )
         (perlutil-perlnow-file-name "") )
    (while (progn 
             (setq perlutil-perlnow-file-name 
                   (read-file-name perlutil-ask-mess perlnow-script-location)
                   )
             (setq perlutil-ask-mess "That name is already in use, use another file name: " )
             (file-exists-p perlutil-perlnow-file-name)))
                                        ; open a buffer associated with the file 
    (find-file perlutil-perlnow-file-name))
  ; Insert the hashbang, a simple header, and make the file executable:
  (perlnow-perlify-this-buffer-simple))

;;;----------------------------------------------------------
(defun perlnow-perlify-this-buffer-simple ()
  "Turn the current buffer into perl window.  This is a simple, 
but inflexible, command that doesn't require template.el.  
It does three things: 
   Adds the hashbang line along with a simple header, 
   Makes the file executable, 
   Goes into cperl-mode using font-lock-mode."
;;; Formerly: perlutil-perlify-this-buffer 
   (interactive)  
    ; insert the hash bang line at the top of the file:
    (goto-char (point-min))
    (insert perlnow-simple-bang-line) 
    (insert "\n")
    (insert "# ")
    ; now, insert a simple header, of the form: 
    ; <programname> - <author's email> 
    ;                 <timestamp>
    (let ((perlutil-file-name-no-path (file-name-nondirectory (buffer-file-name)) ))
      (insert perlutil-file-name-no-path)
        (insert " - " )
        (insert user-mail-address)
        (insert "\n")
      (insert "# ")
        ; Indent so that the date lines up under the email address:
        (let ( (i 0) )
        (while (< i (length perlutil-file-name-no-path) )
          (setq i (1+ i))
          (insert " ")))
        (insert "   ")   ; extend indent passed the " - " on line above
      (insert (current-time-string))
      (insert "\n\n"))
  ; Switch buffer to cperl-mode (whether you like it or not)
  (cperl-mode)  
  ; Turn on font-lock-mode, (if not on already)
  (if (font-lock-mode) 
      (font-lock-mode))
     ; (You might think it should be "if *not* font-lock", but this works.)
  ;; Make the file executable:
  ; Save first: make sure the file really exists before
  ; we change the protections on it
  (save-buffer)
  (let ((perlutil-all-but-execute-mask ?\666) ; Mask to screen out executable file permissions
        (perlutil-file-permissions)
        (perlutil-new-file-permissions))
  (setq perlutil-file-permissions (file-modes (buffer-file-name)))
  (setq perlutil-new-file-permissions 
    (+ (logand perlutil-file-permissions perlutil-all-but-execute-mask) perlnow-executable-setting))
  (set-file-modes (buffer-file-name) perlutil-new-file-permissions))
  (message "buffer is now perlified"))

;;;==========================================================
;;; Documentation utilities
;;;==========================================================
;;; The following functions make it easier to extract documentation 
;;; from perlnow symbols as a first step toward converting 
;;; it to another form such as html.
;;; These are just quick hacks.  (I expect that before I
;;; finished a more general set of tools I'd discover that
;;; someone else has written them already.)
;;; 
;;; Consider spinning off a general package, that extracts help strings
;;; and converts it into (a) html (b) texinfo
;;; In absence of my "symbol-list", could generate one following the 
;;; order of definition in the file.  

(defvar perlnow-symbol-list (list 
                                 "perlnow-documentation"
                                 "perlnow-documentation-installation"
                                 "perlnow-documentation-terminology"
                                 "perlnow-script-location"
                                 "perlnow-module-location"
                                 "perlnow-executable-setting"
                                 "perlnow-perl-script-template"
                                 "perlnow-perl-module-template"
                                 "perlnow-perl-package-name"
                                 "perlnow-package-name-history"
                                 "perlnow-documentation-template-expansions"
                                 "perlnow-minimum-perl-version"
                                 "perlnow-script-run-string"
                                 "perlnow-module-run-string"
                                 "perlnow-run-string"
                                 "perlnow-test-path"
                                 "perlnow-simple-hash-bang-line"
                                 "perlnow-read-minibuffer-map"
                                 "perlnow-run-check"
                                 "perlnow-do-script"
                                 "perlnow-script-using-this-module"
                                 "perlnow-script"
                                 "perlnow-run"
                                 "perlnow-perldb"
                                 "perlnow-set-run-string"
                                 "perlnow-module"
                                 "perlnow-module-two-questions"
                                 "perlnow-h2xs"
                                 "perlnow-script-simple"
                                 "perlnow-perlify-this-buffer-simple"
                                 "perlnow-get-package-name"
                                 "perlnow-insert-spaces-the-length-of-this-string"
                                 "perlnow-full-path-to-module"
                                 "perlnow-make-sure-file-exists"
                                 "perlnow-change-mode-to-executable"
                                 "perlnow-prompt-user-for-file-to-create"
                                 "perlnow-create-with-template"
                                 "perlnow-nix-script-p"
                                 "perlnow-script-p"
                                 "perlnow-module-code-p"
                                 "perlnow-get-package-name-from-module-buffer"
                                 "perlnow-get-package-name-from-man"
                                 "perlnow-vote-on-candidates"
                                 "perlnow-prompt-for-h2xs"
                                 "perlnow-prompt-for-h2xs-again"
                                 "perlnow-one-up"
                                 "perlnow-fixdir"
                                 "perlnow-expand-dots-relative-to"
                                 "perlnow-lowest-level-directory-name"
                                 "perlnow-guess-module-run-string"
                                 "perlnow-get-inc-spot"
                                 "perlnow-perlversion-old-to-new"
                                 "perlnow-full-path-to-h2xs-module"
                                 "perlnow-full-path-to-h2xs-test-file"
                                 "perlnow-blank-out-display-buffer"
                                 "perlnow-module-found-in-INC"
                                 "perlnow-prompt-for-module-to-create"
                                 "perlnow-inc-spot-in-INC-p"
                                 "perlnow-read-minibuffer-complete"
                                 "perlnow-read-minibuffer-complete-word"
                                 "perlnow-read-minibuffer-workhorse"
                                 "perlnow-read-minibuffer-completion-help"
                                 "perlnow-remove-pm-extensions-from-alist"
                                 "perlnow-list-directories-and-modules-as-alist"
                                 "perlnow-list-directory-as-alist"
                                 "perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name"
                                 "perlnow-interesting-file-name-p"
                                 "perlnow-split-module-path-to-dir-and-tail"
                                 )
"Listing of variables and functions in rough order of importance.
Variables first, followed by functions with user-accessible interactive 
functions preceeding the internally used ones.")

(defun perlnow-dump-docstrings-for-symbols (list)
  "Given a LIST of symbol names, dump their documentation strings."
  (dolist (symbol-name list)
    (insert (concat symbol-name ":\n"))
    (let ((symbol (intern-soft symbol-name)))
          (cond ((eq symbol nil)
                 (message "warning: bad symbol-name %s" symbol-name))
                ((functionp symbol)
                 (insert (documentation symbol))
                 (insert "\n\n"))
                (t 
                 (insert (documentation-property symbol 'variable-documentation)))))))


(defun perlnow-dump-docstrings-for-symbols-as-html (list)
  "Given a LIST of symbol names, insert the doc strings with some HTML markup."
  (dolist (symbol-name list)
    (insert (concat "<P><B>" symbol-name "</B>" ":<BR>\n"))
    (let ( doc-string
          (symbol (intern-soft symbol-name)))
          (cond ((eq symbol nil)
                 (message "warning: bad symbol-name %s" symbol-name))
                ((functionp symbol)
                 (setq doc-string
                        (documentation symbol)))
                (t 
                 (setq doc-string 
                       (documentation-property symbol 'variable-documentation))))
          (setq doc-string (perlnow-html-ampersand-substitutions doc-string))
          ; <PRE> if some lines have leading whitespace, else just <P>:
          (cond ((string-match "\n[ \t][ \t]+" doc-string)
                 (insert (concat "<PRE>" doc-string "</PRE></P>\n\n")))
                (t
                 (replace-regexp-in-string "^[ \t]*$" "<BR><BR>" doc-string)
                 (insert (concat doc-string "</P>\n\n"))))
          )))

(defun perlnow-dump-docstrings-for-symbols-as-html-preserving-links (list)
  "Given a LIST of symbol names, insert the doc strings with some HTML markup.
This version tries to preserve links in the documentation as html links. 
STATUS: NOT FINISHED."
  (interactive)
  (dolist (symbol-name list)
    (let* ( doc-string indented-line-pat 
           doc-string-raw
          (symbol (intern-soft symbol-name)))
          (cond ((eq symbol nil)
                 (message "warning: bad symbol-name %s" symbol-name))
                ((functionp symbol)
                 (setq doc-string-raw
                        (documentation symbol t)))
                (t 
                 (setq doc-string-raw
                       (documentation-property symbol 'variable-documentation t))))

          ; Named anchors on every entry for refs to link to
          (insert (format "<A NAME=\"%s\"></A>\n" symbol-name))

          ; Using bold face to indicate a function, italics for variables
          (cond ((functionp symbol)
                 (insert (concat "<P><B>" symbol-name "</B>" ":<BR>\n")))
                (t
                 (insert (concat "<P><I>" symbol-name "</I>" ":<BR>\n"))))

          (setq doc-string (perlnow-html-ampersand-substitutions doc-string-raw))

          ; turn \[(.*)]  into <A HREF="#\1">\1</A>
          (setq doc-string 
                (replace-regexp-in-string "\\\\\\[\\(.*?\\)\\]" ; that's \[(.*?)]   (one hopes)
                                          "<A HREF=\"#\\1\">\\1</A>" 
                                          doc-string))

          ; turn `(.*)'  into <I><A HREF="#\1">\1</A></I>  - note: dot don't match line breaks
          (setq doc-string 
                (replace-regexp-in-string "`\\(.*?\\)']" ; that's `(.*?)'
                                          "<I><A HREF=\"#\\1\">\\1</A></I>" 
                                          doc-string))

          (setq indented-line-pat 
                (concat 
                 ("\n"   ; start of line, in multi-line matching
                  "\\("  ; begin capture to \1  
                  "\\([ ][ ]+\\|\t\\)"  ; indent: 3 spaces or a tab (captures to \2) 
                  ".*?"  ; stuff
                  "\\)"  ; end of capture to \1
                  "\n"   ; end of line
                  ))

          ; Put <PRE> wrapper around indented lines.
          (setq doc-string 
                (replace-regexp-in-string indented-line-pat
                                          "<PRE>\\1</PRE>"
                                          doc-string))
          ; Blank lines => <BR><BR>
          (setq doc-string 
                (replace-regexp-in-string "^[ \t]*$" "<BR><BR>" doc-string))

;          ; <PRE> if some lines have leading whitespace, else just <P>:
;          (cond ((string-match "\n[ \t][ \t]+" doc-string)
;                 (insert (concat "<PRE>" doc-string "</PRE></P>\n\n")))
;                (t
;                 (replace-regexp-in-string "^[ \t]*$" "<BR><BR>" doc-string)
;                 (insert (concat doc-string "</P>\n\n")))

          (insert (concat doc-string "</P>\n\n"))
          )))
;;; Getting close to slick.
;;; TODO
;;; (0)
;;; Forgot to handle the quoted variable references: `perlnow-script-run-string'
;;; (1) 
;;;  List all perlnow-blah, subtract off my list of "important" ones, add the remainder. 
;;;  (Make it impossible to forget to add one to the list.)
;;; (2) 
;;; If a \[blah] is *not* a perlnow-* (best would be to check the list to see if it's there)
;;; Then, run it through the emacs filter: 
;;;    (substitute-command-keys STRING)
;;;    Substitute key descriptions for command names in STRING.
;;; (3) 
;;; Append a TOC of the entire list, as an appendix.  (Too long to lead with).
;;; (4) 
;;; Prepend boiler plate, explaining what the fuck it is? 
;;; Better: create a special purpose template.el template.
;;; Have the code create the html file, then do this fancy doctring insert at point. 
;;; (5) Heuristics to split into multiple HTML pages? 
;;;  E.g. sort symbol names, group by the second term, check the size, do they 
;;;  all fit on one page?  If not, try grouping by the third term...

(defun perlnow-html-ampersand-substitutions (string)
  "Do common html ampersand code substitutions to use this string safely in html."
  (setq string (replace-regexp-in-string "&"   "&amp;"  string))
  (setq string (replace-regexp-in-string "\""  "&quot;" string))
  (setq string (replace-regexp-in-string ">"   "&gt;"   string))
  (setq string (replace-regexp-in-string "<"   "&lt;"   string))
 ;;;  (setq string (replace-regexp-in-string "" "" string))
  )

(defun perlnow-do-docstrings-insertion ()
  "Runs the code that lists *all* of the perlnow doc strings."
  (interactive)
;  (perlnow-dump-docstrings-for-symbols perlnow-symbol-list)
;   (perlnow-dump-docstrings-for-symbols-as-html perlnow-symbol-list)
  (perlnow-dump-docstrings-for-symbols-as-html-preserving-links perlnow-symbol-list)
  )
