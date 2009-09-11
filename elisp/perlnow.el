;;; perlnow.el --- Wed Jan 14 13:45:31 2004

;;; Emacs extensions to speed development of perl code.

;; Copyright 2004,2007 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.244 2009/09/11 05:43:57 doom Exp root $
;; Keywords:
;; X-URL: http://obsidianrook.com/perlnow/


;; And thanks to:
;; Quinn Weaver - bug fixes to identify package names with inside-out OOP modules

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

(defconst perlnow-version "0.4"
  "The version number of the installed perlnow.el package.
Check <http://obsidianrook.com/perlnow/> for the latest.")


(defvar perlnow-documentation t
 "The introductory documentation to the perlnow.el package.
Also see the documentation for:
`perlnow-documentation-installation'
`perlnow-documentation-terminology'
`perlnow-documentation-template-expansions'
`perlnow-documentation-tutorial'
`perlnow-documentation-test-file-strategies'

This package speeds the development of perl code, by making it
easier to jump into coding when an idea strikes. It also includes
some commands to help automate some routine tasks (e.g. running
or checking the code from within emacs).

Perlnow commands typically prompt for a location and/or name,
open a file buffer using an appropriate template.
In the case of scripts the file automatically becomes executable.

Many perlnow.el features require the template.el package plus
some templates for perl development purposes.
See `perlnow-documentation-installation'.

Primarily, perlnow.el provides the following interactive
functions:

\\[perlnow-script] - for creation of new perl scripts.  If
currently viewing some perl module code or a man page for a
perl module, this begins the script with a few lines to use
the module.

\\[perlnow-script-simple] - an older, not quite deprecated
form of \\[perlnow-script] that does not need template.el.

\\[perlnow-module] - for creation of new modules.  Asks for
the location and name of the new module in a single prompt,
using a hybrid form: \"/usr/lib/perl/Some::Module\"

\\[perlnow-object-module] - for creation of new OOP modules.
Much like perlnow-module, but uses a different template.

\\[perlnow-h2xs] - runs the h2xs command, to begin working
on a new module for a CPAN-style distribution.

\\[perlnow-module-starter] - uses module-starter to begin
working on a new module in a CPAN-style distribution.
Currently limited to OOP modules and packages based
on Module::Build.

\\[perlnow-run-check] - does a perl syntax check on the
current buffer, displaying error messages and warnings in
the standard emacs style, so that the next-error command,
\(usually bound to control-x back-apostrophe\)
will skip you to the location of the problem.

\\[perlnow-run] - like the above, except that it actually
tries to run the code, prompting the user for a run string
it if it has not been defined yet.

\\[perlnow-set-run-string] - Allows the user to manually
change the run-string used by perlnow-run.

\\[perlnow-perldb] - runs the perl debugger using the above run string.

\\[perlnow-alt-run] - works just like \\[perlnow-run]
except that it uses the \"alt-run-string\" rather than
the \"run-string\".

\\[perlnow-set-alt-run-string] - Allows the user to manually
change the alt-run-string used by perlnow-alt-run.

A list of the important functions that require template.el:
\\[perlnow-script]
\\[perlnow-module]
\\[perlnow-object-module]
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

Also install template.el if at all possible, because many
features of this package depend on template.el.  The latest
version can be found at:

   http://sourceforge.net/project/showfiles.php?group_id=47369

You'll need some custom perl-oriented template.el templates (\"*.tpl\")
that come with perlnow.el.  Most likely these templates should go in
~/.templates.

Add something like the following to your ~/.emacs file:

   \(require 'template\)
   \(template-initialize\)
   \(require 'perlnow\)

  \(setq `perlnow-script-location'
      \(substitute-in-file-name \"$HOME/bin\"\)\)
  \(setq `perlnow-pm-location'
      \(substitute-in-file-name \"$HOME/lib\"\)\)\n
  \(setq `perlnow-devlocation''
      \(substitute-in-file-name \"$HOME/perldev\"\)\)\n

   \(perlnow-define-standard-keymappings\)

Alternately, if you'd like a different prefix than the
default \"C-c\\\", you can supply it as an argument:

   \(perlnow-define-standard-keymappings \"C-c'\"\)

Or if you prefer, the entire function call can be replaced
with individual definitions like so, to make it easier
to modify them individually:

   \(global-set-key \"\\C-c/s\" 'perlnow-script\)
   \(global-set-key \"\\C-c/m\" 'perlnow-module\)
   \(global-set-key \"\\C-c/o\" 'perlnow-object-module\)
   \(global-set-key \"\\C-c/h\" 'perlnow-h2xs\)
   \(global-set-key \"\\C-c/O\" 'perlnow-module-starter\)
   \(global-set-key \"\\C-c/c\" 'perlnow-run-check\)
   \(global-set-key \"\\C-c/r\" 'perlnow-run\)
   \(global-set-key \"\\C-c/a\" 'perlnow-alt-run\)
   \(global-set-key \"\\C-c/d\" 'perlnow-perldb\)
   \(global-set-key \"\\C-c/R\" 'perlnow-set-run-string\)
   \(global-set-key \"\\C-c/A\" 'perlnow-set-alt-run-string\)
   \(global-set-key \"\\C-c/t\" 'perlnow-edit-test-file\)
   \(global-set-key \"\\C-c/b\" 'perlnow-back-to-code\)
   \(global-set-key \"\\C-c/~\" 'perlnow-perlify-this-buffer-simple\)

Above, the odd prefix \"control-c slash\" has been used because
the C-c <punctuation> bindings are the only places in the keymap
allocated for minor modes, and while the perlnow.el package is
not a minor-mode, it has some aspects in common with them.  The
slash was choosen because it's unshifted and on the opposite side
from the \"c\" \(on typical keyboards\).

You, on the other hand, are free to do whatever you want in
your .emacs, and you might prefer other assignments, such
as using function keys for frequently used commands.
Some examples:

  \(global-set-key [f4] 'perlnow-script\)

  \(add-hook 'cperl-mode-hook
          '\(lambda \(\)
             \(define-key cperl-mode-map [f1] 'perlnow-perl-check\) \)\)

When looking for a good prefix to attach \"perl\" stuff, consider
that \"C-x p\" is used by the p4.el package \(a front-end to the
proprietary perforce version control system\), and you should be
aware that \"M-p\" is used in many contexts for \"history\"
navigation.

Caveats: perlnow.el was developed using GNU emacs 21.1 running
on a linux box \(or GNU/Linux, if you prefer\).  This version
includes bug fixes to get it working with GNU emacs 23.
Reportedly, it does not work with xemacs.")

(defvar perlnow-documentation-terminology t
  "Definitions of some terms used here:

Note: perlnow uses the simplifying assumption that a perl
package is a perl module is a single *.pm file,
Technically multiple packages can be contained in
a single file, but that is not done often in practice.

Why is there such a mess of terminology below?
Because there's a file system name space and a module name space:

   /usr/lib/perl/Modular/Stuff.pm
   /usr/lib/perl/ Modular::Stuff

This makes the answers to simple questions ambiguous:
What is the module called? Stuff.pm or Modular::Stuff?
Where is the module? /usr/lib/perl/Modular or /usr/lib/perl?

The following terms are used here in an attempt at being
more precise:

PM FILE \(or MODULE FILENAME\): the file system's name for
the module file, e.g. /usr/lib/perl/Modular/Stuff.pm

MODULE FILE BASENAME: name of the module file itself, sans
extension: in the above example, \"Stuff\"

PM LOCATION \(or MODULE FILE LOCATION\): directory
portion of module file name, e.g. /usr/lib/perl/Modular/

MODULE NAME or PACKAGE NAME: perl's double colon separated
name, e.g. \"Modular::Stuff\"

INC SPOT or MODULE ROOT or PACKAGE ROOT: a place where perl's
package space begins \(e.g. /usr/lib/perl\). Perl's @INC is a list
of different such \"inc spots\".  These are often named \"lib\".

STAGING AREA: the directory created by h2xs or module-starter
for module development, a hyphenized-form of the module name
e.g. Modular-Stuff. Staging areas contain a module root
\(or \"inc spot\") called \"lib\".

DEV LOCATION: the place where you put your staging areas
\(formerly called the \"h2xs location\" but now it might be
the \"module-starter location\"\).

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
a given module.

TEST PATH: search path to look for test files. Note, can
include relative locations, e.g. \"./t\", but the dot
shouldn't be taken as the current directory.
See: `perlnow-test-path'.

TEST POLICY: the information necessary to know where to
put a newly created test file and what to call it:
1 - the test path dot form, e.g. \"./t\";
2 - the definition of dot e.g. module-file-location vs. inc-spot;
3 - the naming style, e.g. hyphenized vs. base.")

(defvar perlnow-documentation-tutorial t
  "First, see: `perlnow-documentation-installation'.

Depending on how you configure things, you should then have
easy access (perhaps as easy as a single keystroke of a
function key) to some quick short-cuts.  Here's a run down
on how you might use them for different purposes:

 `perlnow-documentation-tutorial-1-script-development'
 `perlnow-documentation-tutorial-2-module-development'
 `perlnow-documentation-tutorial-3-h2xs-module-development'
 `perlnow-documentation-tutorial-4-misc'
 `perlnow-documentation-test-file-strategies'")

(defvar perlnow-documentation-tutorial-1-script-development t
  "Got an idea for a script?  Hit \\[perlnow-script].

This will ask you for the name of the script you want to
write, then kick you into a file buffer with a recommended
code template already filled in.

If you don't like the template, change it \(it should be in
your ~/.templates directory\).  For example, you might
prefer to have \"use strict;\" appear commented out but
ready to be enabled when you know the script is going to be
longer than a dozen lines.

Currently perlnow-script tends to want to put all of your
new scripts in one place, the `perlnow-script-location' that
you've defined for it.  You can, of course, choose a
different place to put a script at creation time: the
default is inserted into the minibuffer so that you can use
it as a starting point to edit into some new location.
Similarly you've also got access to the minibuffer history
to get other starting places.

\(By the way: you do know about the minibuffer history,
don't you?  I didn't until recently.  During a minibuffer
read, you can step back and forth through the history of
things you've entered using: \\[previous-history-element]
and \\[next-history-element]. Typically these are bound to
Alt-p and Alt-n.\)

But every time you use \\[perlnow-script] it's going to try
to put it in the same default location, so \(a\) try and
pick a good default, and \(b\) think about changing it on
the fly if you're going to do a lot of work in a different
place.  You can use \\[set-variable] to set
`perlnow-script-location'.

Okay, so once you're in your new perl script buffer, you can
start coding away.  At any time, you can do a perlnow-run-check
to make sure your syntax is okay.

The command perlnow-run-check acts as a wrapper around the emacs
compile-command facility, feeding it the \"perl -cw\" command.
Once you do the check, the errors and warnings will be listed in
another buffer, and doing a \"next-error\" will rotate you
through these, skipping you directly to the point in the code
where the problem was reported.  By default, one runs \"next-error\"
via \"control-x back-apostrophe\"; and it looks like
your current binding is: \\[next-error]

Alternately, you might skip \\[perlnow-run-check] and
go straight to \\[perlnow-run], which will
\(the first time through\) then ask you how you want to
run the script. The default command line is usually just
\"perl <scriptname>\"; but you can append whatever
arguments and re-directs you like.  Once a run-string
is defined for that file buffer it will stop asking you
this question, though you can change the run string later
at any time with \\[perlnow-set-run-string].

As with \\[perlnow-run-check], \\[perlnow-run] reports any
problems in another buffer \(mixed in with the output from the
program\), once again letting you do the \\[next-error] trick to
jump to where you need to be.

Should you want to use the perl debugger, I suggest using
\\[perlnow-perldb], rather than \\[perldb] directly.  The perlnow
wrapper uses the `perlnow-run-string' you've defined, which will
be different for each script.  If you use the perldb command
directly, you'll notice that the default is just however you ran
it last.  If you're switching back and forth between working on
two scripts, that default is going to be wrong a lot.

The next subject, developing perl modules:
  `perlnow-documentation-tutorial-2-module-development'")


(defvar perlnow-documentation-tutorial-2-module-development t
   "When you're interested in writing a module, the procedure
is similar to script development:
  `perlnow-documentation-tutorial-1-script-development'

You have your choice of three ways of beginning work
on a new module:

For proceedural modules:             \\[perlnow-module]
For object-oriented modules:         \\[perlnow-object-module]
For cpan-style modules:
   Using ExtUtils::MakeMaker         \\[perlnow-h2xs]
   Using Module::Build (by default)  \\[perlnow-module-starter]

The first two are very similar, they just use a different
template (the OOP version is simpler, there being no need
for use Exporter there).  Both ask you for the name and
location of the module you want to create in a single
prompt, asking for an answer in a hybrid form like:

  /home/hacker/perldev/lib/New::Module

Here the module location \(really, a \"module root\"
location, or \"inc spot\", see `perlnow-documentation-terminology')
is entered in the usual file-system form \(in this example,
it is \"/home/hacker/perldev/lib/\"\) and the module name
is given using perl's double-colon separated package name notation
\(in this example, \"New::Module\"\).

The default for the module location is given by the variable
`perlnow-pm-location' which should be set in
your .emacs as indicated in `perlnow-documentation-installation'.
It can also be modified on the fly with \\[set-variable].

Tab and space completion works while navigating the previously
existing part of the path \(including the part inside the package
name space\).  When you hit enter, it will create whatever
intervening directories it needs, after first prompting to make sure
it's okay.

Now I have worked long and hard on getting this single-prompt
method of entering this information, and I'm very proud of
it, and I think it's wonderfully elegant, so the way
these things go the odds are good that you will hate it.

If so, you can use the older form of this command,
\\[perlnow-module-two-questions].  It gets the same information,
but does it by asking a separate question for where and what.
Auto-completion works on the \"where\" question, but not at all
for the module name.

Note that one of the advantages of the \\[perlnow-run-check]
command for doing syntax checks is that it works on module
code just as well as on scripts: you don't need to have a
method of running the module to clean up the syntactical bugs.

If you do a \\[perlnow-run] in a module buffer it will \(a\)
perform an elaborate search to try and find a test file for the
module then \(b\) ask you for the name of a script to run that
uses the module.

If you don't have either yet, you can first run \\[perlnow-edit-test-file]
to create a test file for the module, or \\[perlnow-script]
which will create an ordinary script using the module.

Both of these commands will create files with a
\"use <module name>\" line filled in.  If the module is not in your
@INC search path, it will also add the necessary \"FindBin/use lib\"
magic to make sure that the script will be able to find the module.


If you skip back to the original module buffer, and do a \\[perlnow-run],
you'll notice that the script you just created has become the default
for the way the code in the module gets run.

Both the script and test creation commands capture the name of
the nearest \"sub\" and push it to the kill-ring, where it can
easily be retrieved via \\[yank].

But remember in order for that sub to be accessible, you
might need to do some chores like add the sub name to the
module's EXPORT_TAGS list, and then add it to a qw() list
appended to the \"use <package-name>\" inside the
script.

The module template provided with perlnow puts some useful
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

Next, the cpan-style approach to module development:
  `perlnow-documentation-tutorial-3-cpan-style-module-development'")

(defvar perlnow-documentation-tutorial-3-cpan-style-module-development t
  "There's another completely different style of perl module development
from the one discussed in: `perlnow-documentation-tutorial-2-module-development';
which is oriented toward CPAN-style distributions.
These are created either with module-starter or the older h2xs program,
either of which can be run from within emacs using
\\[perlnow-module-starter] or \\[perlnow-h2xs].

This will ask you two questions, \(1\) where do you want to put
the staging area that h2xs creates, and \(2\) what do you want to
call this module.  The first question defaults to the
customizable variable `perlnow-dev-location'.

Then the commands \\[perlnow-module-starter] or \\[perlnow-h2xs]
will run the appropriate external program, and then leave you
with two windows open, one showing the module file buffer, the
other showing the default test file for the module.

Note that \\[perlnow-module-starter] defaults to using the newer
Module::Build framework, while \\[perlnow-h2xs] defaults to using
the ExtUtils::MakeMaker approach.

\(TODO the perlnow-module-starter default can be changed by using... \)

If you do a \\[perlnow-run] inside of a cpan-style module, it
will identify it and use a different run string: \".Build test\"
in the case of Module::Build or \"make test\" in the case of
ExtUtils::Makemaker.

Next, the template naming convention:
 `perlnow-documentation-tutorial-4-template-naming-convention'")



(defvar perlnow-documentation-tutorial-4-template-naming-convention t
  "There's a convention for naming templates so that perlnow
can find appropriate ones for different cases.  For example, if
you run \\[perlnow-module-starter] using the default settings,
it will use two templates named:

  TEMPLATE.perlnow-modstar-module_build-object-pm.tpl
  TEMPLATE.perlnow-modstar-module_build-object-pm-t.tpl

Here \"modstar\" corresponds to \"module_starter\"
\(as opposed to \\[] or \\[] \).

\"module_build\" means it's for a Module::Build cpan-style
distribution \(as opposed to \"module_install\" or
\"extutils_makemaker\"\).

\"object\" means it's for OOP development (as opposed to \"exporter\").

This naming convention is new \(as of version 0.5\), and it
is not yet universally respected by all perlnow commands.
That can be expected by version 0.6.

Next, everyone's favorite subject, \"Misc\":
 `perlnow-documentation-tutorial-5-misc'")


(defvar perlnow-documentation-tutorial-5-misc t
  "Misc topic 1 - starting from man:

A typical unix-style box these days will have the documentation for
perl modules installed as man pages, which can be most simply read
from inside of emacs with the \\[man] or \\[woman] command.

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
same kind of buffer, the \\[perlnow-script] command
will work with that also.\)

Misc topic 2 - perlify:

In addition to the old-style non-template.el fallback:
\\[perlnow-script-simple], there's another command
that's somewhat similar called: \\[perlnow-perlify-this-buffer-simple].
The \"perlify\" concept is that you can use whatever
habits you're used to to begin working on a script \(e.g.
go into dired, choose a directory, choose a file name
and open it there\), and *then* you can run \"perlify\"
and insert a simple code template and make the file executable.

Originally I found that approach to be a little easier to get
used to than the \\[perlnow-script] approach, but
pretty quickly I abandoned it and switched over.

Note that template.el plus a good perl template, plus that
new emacs 21 trick for making scripts executable
automatically all gets you very close to having this
functionality without any help from perlnow.el... except for
one little gotcha: most of us do not use a standard file
extension (like '.pl') on our perl scripts.  That makes it a
little hard for template.el to spot that you're creating
one.  Though if you can get into the habit of doing a
\\[template-new-file] instead of \\[find-file], and don't
mind selecting the correct template after you enter the file
name then you're pretty much there.

Misc topic 3 - the \"alternative\" way of running a script:

There is an experimental feature that allows
easy access to two different ways of running some code.
In addition to the commands \\[perlnow-run] and
\\[set-perlnow-run-string] commands there are now
\\[perlnow-alt-run] and \\[set-perlnow-alt-run-string].
The \"alt-run\" commands behave identically to the \"run\"
commands, but they use a different buffer-local variable
to store the run string.  The developer can then do
things like use the \\[perlnow-alt-run] command to run
a general regression test for an entire module, but
use \\[perlnow-run] to run a small test that just exercises
whatever feature is currently under development.
It's often useful to have a simple, fast-running test
that you use frequently, and a more through battery
of tests on which you can allow a run time of several
minutes because you don't use it as often.

Note that if you need to switch between more than two
run strings, there's always the minibuffer \"history\"
features:  \\[previous-history-element] and
\\[next-history-element] which in-context, you will
typically find bound to Alt-p and Alt-n."

Next:
 `perlnow-documentation-6-test-file-strategies'")

(defvar perlnow-documentation-6-test-file-strategies t
  "As mentioned in a few other places, the \\[perlnow-run] and
\\[set-perlnow-run-string] commands try to find appropriate test
files for perl code buffers.  There's a relatively elaborate
search path for this.  Here's a description of what it looks for
\(but please, don't be suprised if the precedence changes in the
future\):

First of all, test files all end with the \".t\" extension.
There are several possibilities considered for the name of the
basename of the test file.

For example, in the case of \"Modular::Silliness\", the name
might be \"Silliness.t\", or \"Modular-Silliness.t\" or
\"01-Modular-Silliness.t\" \(which is the initial name used by
module_starter\).

Secondly, a test file might be located in the same place that a
module file is located, or it may be located in the module root
location where the module's package name space starts, and of
course, it might be tucked away in a directory called \"t\" which
could be located in either of those places.

This means that there are a number of strategies you might
choose to use for perl module test files that should
work well with perlnow.el.

An example of a good practice (and the default currently used by
perlnow) would be to always use the hyphenized base name form,
and always put test files in a directory called \"t\", a
subdirectory of the place where \".pm\" file is located.
So if you've got a module called \"Modular::Silliness\", which
is really the file: ~/perldev/lib/Modular/Silliness.pm For an
initial test file name, you would have:

  ~/perldev/lib/Modular/t/Modular-Silliness.t

There is a \\[perlnow-edit-test-file] command that will create this
new test file if it does not already exist.  The user defineable
\"test policy\" dictates where these new test files will go.  See
\"test policy\" in `perlnow-documentation-terminology'.

With a different policy, you might put your initial test file
in any of these locations:

  ~/perldev/lib/t/Modular-Silliness.t
  ~/perldev/lib/Modular/t/Silliness.t
  ~/perldev/lib/Modular-Silliness.t
  ~/perldev/lib/Modular/Silliness.t
  ~/perldev/t/Modular-Silliness.t

It is also possible, though not recommended to use a policy that
would do something like this:

  ~/perldev/lib/t/Silliness.t
  ~/perldev/lib/Silliness.t
  ~/perldev/t/Silliness.t

There's too much potential for name collisions if you use
the short \"basename\" form high up in the tree. Modular::Silliness
and Monolithic::Silliness would fight to use the same name.

Note that perlnow \(at least currently\) does not care if you're
consistent about this choice."

Next:
 `perlnow-documentation-7-template-expansions'")




;;;;##########################################################################
;;  User Options, Variables
;;;;##########################################################################


(defun perlnow-fixdir (dir)
  "Fixes the DIR.
Conditions directory paths for portability and robustness.
Some examples:
 '~/tmp'             => '/home/doom/tmp/'
 '~/tmp/../bin/test' => '/home/bin/test/'
"
  (let ((return
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name dir))))))
    return))

; TODO:
; on the following three locations, I'm currently using HOME
; environment variable for a default location, though it's
; expected this will be overridden with a .emacs setting.
; Maybe it would be better to default to something else, possibly:
;   ~/bin  ~/lib
; Maybe, see if they exist, and then use them, if not, silently fall
; back on HOME? (ditto ~/dev).

(defcustom perlnow-script-location (file-name-as-directory (getenv "HOME"))
  "This is the default location to stash new perl scripts.")

(defcustom perlnow-pm-location (file-name-as-directory (getenv "HOME"))
  "This is the default location to stash new perl modules.")

(defcustom perlnow-dev-location (file-name-as-directory perlnow-pm-location)
  "This is the default location to do h2xs/modstar development for CPAN-style packages.")

(defcustom perlnow-executable-setting ?\110
  "The user-group-all permissions used to make a script executable.")

(defvar perlnow-template-location (perlnow-fixdir "$HOME/.templates")
  "Standard location for template.el templates.")
  ;; TODO Question: can I get template.el to tell me the template location?

(defcustom perlnow-perl-script-template
  (concat perlnow-template-location "/" "TEMPLATE.perlnow-pl.tpl")
  "The template that new perl scripts will be created with.")
(put 'perlnow-perl-script-template 'risky-local-variable t)

(defcustom perlnow-perl-module-template
  (concat perlnow-template-location "/" "TEMPLATE.perlnow-pm.tpl")
  "The template that new perl modules will be created with.")
(put 'perlnow-perl-module-template  'risky-local-variable t)

(defcustom perlnow-perl-object-module-template
  (concat perlnow-template-location "/" "TEMPLATE.perlnow-object-pm.tpl")
  "The template that new perl object modules will be created with.")
(put 'perlnow-perl-object-module-template  'risky-local-variable t)

(defcustom perlnow-perl-test-script-template
    (concat perlnow-template-location "/" "TEMPLATE.perlnow-pl-t.tpl")
  "The template that tests for perl scripts will be created with.")
(put 'perlnow-perl-test-template  'risky-local-variable t)

(defcustom perlnow-perl-test-module-template
    (concat perlnow-template-location "/" "TEMPLATE.perlnow-pm-t.tpl")
  "The template that ordinary module perl test scripts will be created with.")
(put 'perlnow-perl-test-template  'risky-local-variable t)

(defcustom perlnow-license-message
  "This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information."
  "Software license message available to templates as LICENSE.
The default value is the traditional boilerplate for open source perl code.")
(put 'perlnow-license-message  'risky-local-variable t) ; cargo cult

(defvar perlnow-perl-script-name nil
  "Used internally to pass the script name to some templates.
Defines the PERL_SCRIPT_NAME expansion.")

(defvar perlnow-perl-package-name nil
  "Used internally to pass the module name to the new module template.
Defines the PERL_MODULE_NAME expansion.")

(defvar perlnow-package-name-history nil
  "The minibuffer history for perl modules accessed by this package.")

(defconst perlnow-slash (convert-standard-filename "/")
  "A \(possibly\) more portable form of the file system name separator.")
; Using this instead of "/", as a stab at portability (e.g. for windows).
; But even if this helps, there are still other places
; dependencies have crept in, e.g. patterns that use [^/].
; (And what about a root of "/" vs "C:\" ?)


;; Defining additional "expansions" for use in template.el templates.
;;
(defvar perlnow-documentation-7-template-expansions t
  "The perlnow template.el templates use some custom
expansions defined in perlnow.el.  A template.el
\"expansion\" is a place holder in the template that
gets replaced by something else when the template is
used.  For example, \(>>>DATE<<<\) will become the
current date.

The perlnow custom expansions:

\(>>>EMAIL_DOT_EMACS<<<\)
This inserts the users email address as determined from
their .emacs setting of the variable `user-mail-address'.

\(>>>PERL_MODULE_NAME<<<\)
becomes the perl module name \(in double-colon
separated form\) when used by \\[perlnow-module]
function.

\(>>>PERL_SCRIPT_NAME<<<\)
becomes the perl script name of the previous
current buffer.  Used in creating test scripts
that need to refer to the current script.

\(>>>MINIMUM_PERL_VERSION<<<\)
The minimum perl version you usually support.  Gets used in
the first line in a perl module, e.g. \"use 5.006;\".
Used by \\[perlnow-module] to insert the value of
`perlnow-minimum-perl-version'.

\(>>>AMERICAN_DATE<<<\)
The current date in the much-derided American format:
MM/DD/YY, where MM and DD do not use leading zeroes.

\(>>>FULL_DATE<<<\)
The current date without abbreviated month name, e.g.
\"August 8, 2009\".

\(>>>TAB<<<\)
Experimental feature: should indent as though the tab
key had been hit.  I suspect that you need to use
\(>>>TAB<<<\) *after* the line of code and not before.

\(>>>PNFS<<<\)
stands for \"PerlNow Filename Spaces\" it should
always insert the same number of spaces as characters
in the name of the file.  This can be used to get formatting
to line up, for example:
   \(>>>FILE<<<\)              \(>>>AUTHOR<<<\)
   \(>>>PNFS<<<\)              \(>>>DATE<<<\)

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
      '("PERL_SCRIPT_NAME" (insert perlnow-perl-script-name) )
      template-expansion-alist))

(setq template-expansion-alist
      (cons
      '("PERL_MODULE_NAME" (insert perlnow-perl-package-name) )
      template-expansion-alist))

(setq template-expansion-alist
      (cons
      '("EMAIL_DOT_EMACS" (insert user-mail-address) )
      template-expansion-alist))

(setq template-expansion-alist
      (cons
      '("PNFS"
        (perlnow-insert-spaces-the-length-of-this-string (buffer-file-name)))
      template-expansion-alist))

(setq template-expansion-alist
      (cons
      '("AMERICAN_DATE" (insert (perlnow-american-date)))
      template-expansion-alist))

(setq template-expansion-alist
      (cons
      '("FULL_DATE" (insert (perlnow-full-date)))
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

(setq template-expansion-alist
      (cons
      '("LICENSE" (insert perlnow-license-message))
      template-expansion-alist))



(defvar perlnow-minimum-perl-version "5.006"
  "The minimum perl version you are interested in supporting.
This is used to define the template expansion of MINIMUM_PERL_VERSION.
Note that perl version numbers jumped from 5.006 to 5.7.0.  As of
this writing, the latest is 5.8.2")
; Defining feature MINIMUM_PERL_VERSION to insert the above as an
; an "expansion" in a template.el template: (>>>MINIMUM_PERL_VERSION<<<);
(setq template-expansion-alist
      (cons
      '("MINIMUM_PERL_VERSION" (insert perlnow-minimum-perl-version))
      template-expansion-alist))
;;; DEBUG note: eval this to erase effects of the above two settings:
;;; (setq template-expansion-alist 'nil)


;;; I am following my instinct and using make-variable-buffer-local
;;; to force the following to always be buffer-local, despite the
;;; admonition in the emacs lisp ref.
;;; (1) this makes the code a little simpler (I don't want to have
;;; to remember to use make-local-variable in different places);
;;; (2) I can't think of a case where the user would be annoyed at
;;; me depriving them of this choice.

;;; TODO refactor - I intensely dislike having separate module and
;;;    script runstrings variables (both of which are almost
;;;    certainly nil) and the one actual run-string.
;;;    This is a problem multiplied by two now with the alt-run-string.

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

;;; Now implementing the "alt-run-string" in addition to
;;; the "run-string": having both allows for
;;; having two separate concurrently defined ways of running the
;;; the perl code in the current buffer.  The heuristics for
;;; guessing what run string to use remain identical.

(defvar perlnow-script-alt-run-string nil
   "The alternative run string for perl scripts, used by \\[perlnow-alt-run].
Leave this set to nil unless you want to override the heuristics
used by \\[perlnow-set-alt-run-string] to determine the way to test
the current script.  This is a buffer local variable, i.e. it
may be set differently for different files.")
(put 'perlnow-script-alt-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-script-alt-run-string)

(defvar perlnow-module-alt-run-string nil
   "The alternative run string for perl modules, used by \\[perlnow-alt-run].
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

(defvar perlnow-associated-code nil
  "Associated code for the current buffer (presumably a test file).
Used by \\[perlnow-back-to-code].")
(put 'perlnow-associated-code  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-associated-code)

(defcustom perlnow-test-path (list "." "../t" "./t")
   "List of places to look for test scripts.
These use a dot notation to express relative location,
though rather than interpreting \".\" as the current
directory, it will be interpreted as either the
module root or the module location.")
(put 'perlnow-test-path  'risky-local-variable t)

;; TEST POLICY: the information necessary to know where to
;; put a newly created test file and what to call it:
;; 1 - the test path dot form, e.g. \"./t\";
;; 2 - the definition of dot e.g. module pm-location vs. inc-spot;
;; 3 - the naming style, e.g. hyphenized vs. base.")

(defcustom perlnow-test-policy-test-location  "./t"
  "Test location for newly created test files.
May be specified using a \"dot form\", relative to
`perlnow-test-policy-dot-definition'.  E.g. \"./t\",
\"../t\", \"~/my_test_files\" etc.
Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")

(defcustom perlnow-test-policy-dot-definition  "fileloc"
  "The meaning of the \".\" in `perlnow-test-policy-test-location'.
Currently a string with two allowed values: \"fileloc\" or \"incspot\".
If \"fileloc\", we want to specify a location relative to the file's
file system path.  If \"incspot\" we want to specify a location
relative to the root of the module name space. E.g. for \"Modular::Stuff\"
the fileloc is the directory \"Modular\", and the incspot is
the location of the directory \"Modular\".
Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")

(defcustom perlnow-test-policy-naming-style "hyphenized"
  "Naming style to be used in creating a new test file for a module.
There are only two naming styles provided \"hyphenized\"
and \"basename\".  E.g. for \"Modular::Stuff\" the hyphenized
test file name would be \"Modular-Stuff.t\", the basename style
would be \"Style.t\".
Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")

(defcustom perlnow-module-starter-builder "Module::Build"
  "Base module for a cpan-style distribution.
Allowed values: \"Module::Build\", \"Module::Install\", \"ExtUtils::MakeMaker\".")


(defcustom perlnow-perl-path "perl"
  "Set this to provide a hint about your preferred perl binary.
For example, make it \"/usr/local/bin/perl\" if you would rather
use that than the system's perl.  Defaults to just \"perl\"
\(and let's the path sort it out\).  Note: this is used only in some
cases, e.g. \\[perlnow-module-starter], where possible perlnow
uses whatever is specified in the hash-bang line.")

(defcustom perlnow-module-style "object"
  "Type of module you usually prefer to create, e.g. \"object\", or \"exporter\".
Defaults to \"object\", which for better or worse is the current
standard.  Used by some routines, such as
\\[perlnow-module-starter], to choose a code template.
Note, there is no restriction on the allowed values here, any
arbitrary string can be used, provided you have appropriate code
templates that use it.")


(defcustom perlnow-simple-hash-bang-line "#!/usr/bin/perl -w"
  "A typical hash bang line for perl code.
Used only by the somewhat deprecated \"simple\" functions:
\\[perlnow-script-simple] \\[perlnow-perlify-this-buffer-simple]")

(defvar perlnow-getter-prefix "get_"
  "Defines the naming convention for getters for object-oriented code.
Editorial: the default setting in perlnow.el is \"get_\", because that's
very common, but if you never use the (now deprecated) mutators, doesn't
it make more sense to use no prefix on getters?")

(defvar perlnow-setter-prefix "set_"
  "Defines the naming convention for setters for object-oriented code.")

;;;==========================================================
;;; User Commands
;;;==========================================================

;;;==========================================================
;;; set-up functions

(defun perlnow-define-standard-keymappings ( &optional prefix )
  "Quickly define some recommended keymappings for perlnow
functions.  By default, perlnow.el makes no changes to the user's
keymap. This function is provided to make it easy for you
to adopt a standard set of keymappings, but they're not
forced on you.  Note: these all use the \"C-c/\" prefix.  A few
mappings are also included for useful functions that are defined
outside of the perlnow.el package: cperl-perldoc-at-point,
comment-region and narrow-to-defun."
 ; TODO - Would be even cooler if it looked for and warned
 ; about possible collisions...
  (interactive)
  (unless prefix (setq prefix "\C-c/"))
  (global-set-key (format "%ss" prefix) 'perlnow-script)
  (global-set-key (format "%sm" prefix) 'perlnow-module)
  (global-set-key (format "%so" prefix) 'perlnow-object-module)
  (global-set-key (format "%sh" prefix) 'perlnow-h2xs)
  (global-set-key (format "%sO" prefix) 'perlnow-module-starter)
  (global-set-key (format "%sc" prefix) 'perlnow-run-check)
  (global-set-key (format "%sC" prefix) 'perlnow-run-check-thorough)
  (global-set-key (format "%sr" prefix) 'perlnow-run)
  (global-set-key (format "%sa" prefix) 'perlnow-alt-run)
  (global-set-key (format "%sd" prefix) 'perlnow-perldb)
  (global-set-key (format "%sR" prefix) 'perlnow-set-run-string)
  (global-set-key (format "%sA" prefix) 'perlnow-set-alt-run-string)
  (global-set-key (format "%st" prefix) 'perlnow-edit-test-file)
  (global-set-key (format "%sb" prefix) 'perlnow-back-to-code)
  (global-set-key (format "%s~" prefix) 'perlnow-perlify-this-buffer-simple)
  (global-set-key (format "%s1" prefix) 'cperl-perldoc-at-point)
  (global-set-key (format "%s#" prefix) 'comment-region)
  (global-set-key (format "%sN" prefix) 'narrow-to-defun))

;;;==========================================================
;;; functions to run perl scripts

; TODO scrape hashbang (if present), and just default to this: perlnow-perl-path
; TODO write a routine to tildicize file here.  clean-up display?
; perhaps even better, thinking about changing the directory...
; TODO probe for perlcritic, skip it if it's not installed.
; TODO And don't try to run a binary if it's been set to blank or nil, eh?
(defun perlnow-run-check (arg)
  "Run a perl check on the current buffer.
This displays errors and warnings in another window, in the usual
emacs style: After running this, you can skip to the location of
the next problem with \\\[next-error]\n This command is like
\\\[cperl-check-syntax] with one less prompt \(also, it does not
require mode-compile.el\).  When run with a prefix argument
\(i.e. after hitting C-u\), it runs a more elaborate suite of
checks, including podchecker and perlcritic."
  (interactive "P")
  (let* ( (file (buffer-file-name))
          (perl perlnow-perl-path)
          (podchecker "podchecker")   ;; TODO expose as var
          (perlcritic "perlcritic")   ;; TODO expose as var
          )
    (save-buffer)
    (cond ( (not arg) ; no prefix
            (setq compile-command
                  (format "%s -Mstrict -cw \'%s\'" perl file))
            )
          (t ; C-u prefix
           (setq compile-command
                 (concat
                  (format "%s -Mstrict -cw \'%s\'" perl file)
                  "; "
                  (format "%s \'%s\'" podchecker file)
                  "; "
                  (format "%s --nocolor --verbose 1 \'%s\'" perlcritic file)
                  ))
           ))
    (message "compile-command: %s" compile-command)
    (compile compile-command);; this just returns buffer name "*compilation*"
    ))

(defun perlnow-run (runstring)
  "Run the perl code in this file buffer.
This uses an interactively set RUNSTRING determined from
`perlnow-run-string' which may have been set by using
\\[perlnow-set-run-string].  If `perlnow-run-string' is nil,
\\[perlnow-set-run-string] is called automatically.\n
The run string can always be changed later by running
\\[perlnow-set-run-string] manually."
  (interactive
   (let (input)
   (if (eq perlnow-run-string nil)
       (setq input (perlnow-set-run-string))
     (setq input perlnow-run-string))
   (list input)
   ))
  (compile runstring))


(defun perlnow-alt-run (altrunstring)
  "Run the perl code in this file buffer.
This uses an interractively set ALTRUNSTRING determined
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
  (perlnow-run altrunstring)) ; Note: uses perlnow-run rather than running compile directly


(defun perlnow-perldb (runstring)
  "Run the perl debugger on the code in this file buffer.
This uses an interactively set RUNSTRING determined from
`perlnow-run-string' which may have been set by using
\\[perlnow-set-run-string].  If `perlnow-run-string' is nil,
\\[perlnow-set-run-string] is called automatically.
It can always be changed later by running \\[perlnow-set-run-string]
manually.  \n
There's a big advantage that this command has over running
\\[perldb] directly: you can have different `perlnow-run-string'
settings for different file buffers \(that is to say it's a buffer
local variable\).  Unfortunately \(as of this writing\) \\[perldb]
used directly always re-uses it's previous run-string as a
default, and that's guaranteed to be wrong if you've switched
to a different file."
  (interactive
   (let (input)
     (if (eq perlnow-run-string nil)
         (setq input (perlnow-set-run-string))
       (setq input perlnow-run-string))
     (list input)
     ))
  (let ((modified-runstring
         (replace-regexp-in-string "\\bperl " "perl -d " runstring)))
    (perldb modified-runstring)))


(defun perlnow-set-run-string ()
  "Prompt the user for a new run string for the current buffer.
This sets the global variable `perlnow-run-string' that \\[perlnow-run]
will use to run the code in future in the current buffer.
The user needs to run this directly to manually change the run
string, but it is also used indirectly by the \\[perlnow-run]
if the run string is not yet defined.

From within a program, it's probably best to set some variables
directly, see `perlnow-script-run-string' and `perlnow-module-run-string'.
This function uses \\\[perlnow-module-code-p] to see if the code looks like a
module (i.e. does it have a package line), otherwise it
assumes it's a perl script."
;; And if it's not perl at all, that's your problem: the obvious
;; tests for perl code, like looking for the hash-bang,
;; aren't reliable (perl scripts need not have a hash-bang
;; line: e.g. *.t files, perl on windows...).
;;; TODO - would be better to do a script-p, set a runstring based on that,
;;; and then have a fall through section that tries to verify if it's some
;;; sort of test script ("use Test"?), and otherwise either fail with warning,
;;; or prompt the user ask them what they think they're doing.
;;; Ah, another way out: run a perl -c on the buffer, and if it fails,
;;; tell the user it ain't passing perl check (is it even perl code?).
;;; (( why not check to see if you're in a perl mode, eh?
;;;    or just *restrict these commands to those modes* ))
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
         (setq perlnow-script-run-string
               (perlnow-guess-script-run-string))
         ))
     ; ask user how to run this script (use as default next time)
     (setq perlnow-script-run-string
           (read-from-minibuffer
            "Set the run string for this script: "
            perlnow-script-run-string))
     ; tell perlnow-run to do it that way
     (setq perlnow-run-string perlnow-script-run-string))))


(defun perlnow-set-alt-run-string ()
  "Prompt the user for a new alternative run string for the current buffer.
This sets the global variable `perlnow-alt-run-string' that \\[perlnow-alt-run]
will use to run the code in future in the current buffer.
Frequently, the user will prefer to use \\[perlnow-alt-run] and let it
run this command indirectly if need be; however using this command
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
     ; set-up a decent default value
     (unless perlnow-module-alt-run-string
       (progn
         (setq perlnow-module-alt-run-string
               (perlnow-guess-module-run-string))))
     ; ask user the alternative way to run this module (use as default next time)
     (setq perlnow-module-alt-run-string
           (read-from-minibuffer
            "Set the alternative run string for this module: "
            perlnow-module-alt-run-string))
     ; tell perlnow-alt-run how to do it
     (setq perlnow-alt-run-string perlnow-module-alt-run-string))
   (t  ;;  assume it's a script since it's not a module.
     ; set-up intelligent default alt run string
     (unless perlnow-script-alt-run-string
       (progn
         (setq perlnow-script-alt-run-string
               (perlnow-guess-script-run-string))
         ))
     ; ask user the alternative way to run this script (use as default next time)
     (setq perlnow-script-alt-run-string
           (read-from-minibuffer
            "Set the alternative run string for this script: "
            perlnow-script-alt-run-string))
     ; tell perlnow-alt-run to do it that way
     (setq perlnow-alt-run-string perlnow-script-alt-run-string))))


;;;==========================================================
;;; user level creation functions (script, module, h2xs...)


(defun perlnow-script (script-name)
  "General purpose command to quickly jump into coding a perl script.
This prompts the user for the new SCRIPT-NAME, and then uses
the current buffer to get some hints about what lines you might
like to have in the new script to start coding with.
If you've been looking at some perl module code -- or a man page
documenting a perl module -- it will give you a \"use\" line to include
that module.  If the module is not in perl's @INC array, it will also
insert the appropriate \"FindBin\" & \"use lib\" lines so that the script
can find the module."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (require 'template)
  (let ( package-name)
    (cond
     ((setq package-name (perlnow-get-package-name-from-module-buffer))
       (let* ( (pm-file (buffer-file-name))
               (pm-location (file-name-directory pm-file))
               (inc-spot (perlnow-get-inc-spot package-name pm-location)) )
        (setq perlnow-perl-package-name package-name) ; global used to pass value into template
        (perlnow-do-script-from-module script-name package-name inc-spot) ))

      ((setq package-name (perlnow-get-package-name-from-man))
        (setq perlnow-perl-package-name package-name) ; global used to pass value into template
        (perlnow-do-script-from-module script-name package-name))
      (t ; no package name found, so we're working with a script
         ; (someday, might use perlnow-script-p)
       (perlnow-do-script script-name)))))

;;;   TODO
;;;    Someday: check if module is in INC (when starting from man)
;;;    and report any problems, say by
;;;    Inserting comment in code file near use lib:
;;;         # Currently not found in @INC. Installed correctly?
;;;    Could use this to do the check:
;;;      (setq pm-file (perlnow-module-found-in-INC package-name))
;;;         ; given colon-ized, returns first pm found, or nil if none



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
is used to determine where perl's package namespace begins.  \n
The \".pm\" extension is assumed and need not be entered. \n
If the module exists already, the user is asked for another name. \n
The location for the new module defaults to the global
`perlnow-pm-location'. The default location is used as the initial
contents of the minibuffer, so that it may be edited at time of module
creation."
;;; Formerly named: perlnow-prompt-for-new-module-in-one-step
  (interactive
   (let ((initial perlnow-pm-location)
         ; The keymap is key: transforms read-from-minibuffer.
         (keymap perlnow-read-minibuffer-map)
         (history 'perlnow-package-name-history)
         result filename return
         )
     (setq result
           (read-from-minibuffer
            "New module to create \(e.g. /tmp/dev/New::Mod\): "
                                 initial keymap nil history nil nil))
     (setq result (replace-regexp-in-string "\.pm$" "" result)) ; remove accidentally typed ".pm"
     (setq filename
           (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm"))
     (while (file-exists-p filename)
       (setq result
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
                                 result keymap nil history nil nil))
       (setq filename
             (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm")))

     (setq return
           (perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name result))
     return))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module inc-spot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))



(defun perlnow-object-module (inc-spot package-name)
  "Quickly jump into development of a new perl OOP module.
In interactive use, gets the path INC-SPOT and PACKAGE-NAME
with a single question, asking for an answer in a hybrid form
like so:
   /home/hacker/perldev/lib/New::Module
This works much like \\[perlnow-module], except that it uses
a different template.\n
The location for the new module defaults to the global
`perlnow-pm-location'."
;;; Mutated from perlnow-module
  (interactive
   (let ((initial perlnow-pm-location)
         (keymap perlnow-read-minibuffer-map) ; The keymap is key: transforms read-from-minibuffer.
         (history 'perlnow-package-name-history)
         result filename return
         )
     (setq result
           (read-from-minibuffer
            "New OOP module to create \(e.g. /tmp/dev/New::Mod\): "
                                 initial keymap nil history nil nil))
     ;;; TODO: check 'result', if it has a .pm file already, strip it first.
     (setq filename (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm"))
     (while (file-exists-p filename)
       (setq result
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
                                 result keymap nil history nil nil))
     ;;; TODO: check 'result', if it has a .pm file already, strip it first.
       (setq filename (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm")))
     (setq return
           (perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name result))
     return))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module inc-spot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-object-module-template)))



(defun perlnow-h2xs (dev-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the DEV-LOCATION  \(the place where
h2xs will create the \"staging area\"\) and the PACKAGE-NAME \(in perl's
double-colon separated package name form\)."
; Because default-directory is the default location for (interactive "D"),
; I'm doing the interactive call in stages: this way can change
; default-directory momentarily, then restore it. Uses the dynamic scoping
; of elisp's "let" (which is more like perl's "local" than perl's "my").
  (interactive
    (let ((default-directory perlnow-dev-location))
      (call-interactively 'perlnow-prompt-for-cpan-style)))
  (setq dev-location (perlnow-fixdir dev-location))  ;; just playing safe
  (unless (file-exists-p dev-location)
    (make-directory dev-location t))
  (let* ( display-buffer ; buffer object
          (h2xs-module-file "")
          (h2xs-test-file   "")
          (h2xs-staging-area "")
          (window-size 14)
          )
    (setq display-buffer (get-buffer-create "*perlnow-h2xs*"))
    ;Bring the *perlnow-h2xs* display window to the fore (bottom window of the frame)
    (perlnow-show-buffer-other-window display-buffer window-size t)
    (perlnow-blank-out-display-buffer display-buffer t)
    (let ((default-directory dev-location))
     ; A typical h2xs run string:  h2xs -AX -n Net::Acme -b 5.6.0
      (call-process "h2xs"
                nil
                display-buffer      ; must be buffer object?
                nil
                "-AX"
                (concat "-n" package-name)
                (concat "-b"
                        (perlnow-perlversion-old-to-new perlnow-minimum-perl-version))))
  (setq h2xs-staging-area (perlnow-staging-area dev-location package-name))
  (perlnow-cpan-style-build h2xs-staging-area)
  (setq h2xs-module-file (perlnow-full-path-to-cpan-style-module dev-location package-name))
  (find-file h2xs-module-file)
  (search-forward "# Preloaded methods go here.")
  (forward-line 1)
  ; Also  open the *.t file
  (setq h2xs-test-file (perlnow-full-path-to-dev-test-file h2xs-staging-area))
  (perlnow-open-file-other-window
      h2xs-test-file
      window-size)  ; same number of lines as above.  Note: leaving args template and switchback nil.
  (funcall (perlnow-lookup-preferred-perl-mode))
;;  (search-forward "BEGIN { plan tests => 1") ;; worked with older versions of h2xs?
;;  this works now (Sun Aug 23 13:10:48 2009) but why not just go to bottom?
;;  (search-forward "# Insert your test code below, the Test::More module is use()ed here so read
;; # its man page ( perldoc Test::More ) for help writing this test script.
;;")
  (goto-char (point-max))
  (other-window 1)
  ))

(defun perlnow-module-starter (modstar-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the MODSTAR-LOCATION  \(the place where
module-starter will create the \"staging area\"\) and the PACKAGE-NAME
\(in perl's double-colon separated package name form\)."
   ; Because default-directory is the default location for (interactive "D"),
   ; I'm doing the interactive call in stages: this way can change
   ; default-directory momentarily, then restore it.
   ; Uses the dynamic scoping of elisp's "let"
  (interactive
   (let ((default-directory perlnow-dev-location))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (setq modstar-location (perlnow-fixdir modstar-location))

  (unless (file-exists-p modstar-location)
    (make-directory modstar-location t))
  (let* ( (modstar-template-tag "modstar")
          (display-buffer) ; buffer object
          (modstar-module-file "")
          (modstar-test-file   "")
          (modstar-staging-area "")
          (window-size 14)     ;; number of lines for the *.t file buffer
          (module-style perlnow-module-style)
          (builder-code
           (downcase (mapconcat 'identity (split-string perlnow-module-starter-builder  "::") "_")))
          )
    (setq display-buffer (get-buffer-create "*perlnow-module-starter*"))
    ;Bring the *perlnow-module-starter* display window to the fore (bottom window of the frame)
    (perlnow-show-buffer-other-window display-buffer window-size t)
    (perlnow-blank-out-display-buffer display-buffer t)

    (let* ((default-directory modstar-location)
           (modstar-cmd (perlnow-generate-module-starter-cmd  package-name modstar-location ))
           )
      (shell-command modstar-cmd display-buffer nil)

      (setq modstar-staging-area (perlnow-staging-area modstar-location package-name))
      (perlnow-cpan-style-build modstar-staging-area)
      (setq modstar-module-file (perlnow-full-path-to-cpan-style-module modstar-location package-name))

      ;; create a module and test file using appropriate templates,
      ;; and swap the module file in place of the one module-starter creates
      (let* ( (template-name
               (format
                "%s/TEMPLATE.perlnow-%s-%s-%s-pm.tpl"
                perlnow-template-location
                modstar-template-tag
                builder-code
                module-style
                ))
              (test-template
               (format
                "%s/TEMPLATE.perlnow-%s-%s-%s-pm-t.tpl"
                perlnow-template-location
                modstar-template-tag
                builder-code
                module-style
                ))
              )
        (require 'template)
        (setq perlnow-perl-package-name package-name) ; global used to pass value into template

        (delete-file modstar-module-file)
        (perlnow-create-with-template modstar-module-file template-name)

        ; Also open the *.t file
        (setq modstar-test-file
              (perlnow-full-path-new-module-starter-test-file modstar-staging-area package-name))

        (perlnow-open-file-other-window
           modstar-test-file
           window-size
           test-template
           t )

        (funcall (perlnow-lookup-preferred-perl-mode))
        ))))

(defun perlnow-generate-module-starter-cmd (module-name location)
  "Generate shell command string to run module-starter.
Creates a standard layout for development of a perl module named MODULE-NAME
in the directory LOCATION.
Get's the user's full name from the emacs function user-full-name
and the email address from the variable user-mail-address."
  (let* ( (author-name (user-full-name))
          (hyphenated (mapconcat 'identity (split-string module-name "::") "-"))
          (subdir (concat location "/" hyphenated))
          (perlnow-module-starter-cmd
           (format
            (concat
            "module-starter "
            "--module=\"%s\" "
            "--author=\"%s\" "
            "--email=\"%s\" "
            "--builder=\"%s\" "
            "--license=\"%s\" "
            "--dir=\"%s\"")
            module-name
            author-name
            user-mail-address
            perlnow-module-starter-builder
            perlnow-perl-path
            subdir
            )))
    perlnow-module-starter-cmd))



;;;==========================================================
;;; Older (if not quite deprecated) user level creation commands

(defun perlnow-script-using-this-module (script)
  "Jump quickly into a new SCRIPT that uses the current module code.
If the module is not in perl's search path \(@INC\), then an
appropriate \"use lib\" statement will be added. \n
Note: if multiple packages exist in the file \\(and that's
never really done\\) then this function will see the first
package name."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (require 'template)
  (let* ( (pm-file (buffer-file-name))
          (pm-location (file-name-directory pm-file))
          (package-name (perlnow-get-package-name-from-module-buffer))
          (inc-spot (perlnow-get-inc-spot package-name pm-location))
          )
    (unless package-name
      (error "%s" "This file doesn't look like a perl module (no leading package line)."))

    (perlnow-do-script-from-module script package-name inc-spot)))



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
   (let ((default-directory perlnow-pm-location))
     (call-interactively 'perlnow-prompt-for-module-to-create)))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module inc-spot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))

;;;==========================================================
;; The "simple" functions.  Older code that doesn't use template.el.
;;;==========================================================

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


(defun perlnow-perlify-this-buffer-simple ()
  "Turn the current buffer into perl window \(without template.el\).
This is a simple, but inflexible, command that doesn't
require template.el.
It does three things:
   Adds the hashbang line along with a simple header,
   Makes the file executable,
   Goes into cperl-mode using font-lock-mode."
;;; Formerly: perlutil-perlify-this-buffer
   (interactive)
    ; insert the hash bang line at the top of the file:
    (goto-char (point-min))
    (insert perlnow-simple-hash-bang-line)
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


(defun perlnow-edit-test-file (testfile)
   "Find \(or create\) an appropriate TESTFILE for the current perl code.
This command follows this process:
  o Uses the given testfile (if run non-interactively).
  o Checks if the code looks like a module or a script:
    Scripts have a modified test policy: always use naming style
    \"basename\", and dot-def \"fileloc\".
  o Look for an existing file in place dictated by test policy.
  o If not, Searches the test path, looks for an existing file there
    (If more than one is found it will complain.)
  o If no existing file is found, creates one as determined by the
    test policy.
  o Finally, the run string for the current buffer is set so that
    it will run this test.
The test policy is defined by this trio of variables:
`perlnow-test-policy-test-location', e.g. \".\", \"./t\", \"../t\", etc.
`perlnow-test-policy-dot-definition' i.e.  \"fileloc\" or \"incspot\"
`perlnow-test-policy-naming-style'   i.e. \"hyphenized\"or \"basename\"."  ;; TODO  'numeric'
; Remember the *runstring* is a bit different for
; an cpan-style module than a regular module.
  (interactive
   (list (perlnow-get-test-file-name)))
  ; set some buffer-local variables before we go any where
  (setq perlnow-run-string (concat "perl " testfile))
  (setq perlnow-associated-code testfile)
  (let (package-name new-file-p original-code)
    (setq new-file-p (not (file-exists-p testfile)))
    (setq original-code (buffer-file-name))
    (cond
     ; if module
     ((setq package-name (perlnow-get-package-name-from-module-buffer))
      ; define module inc-spot now, before opening test file buffer
      (let* ( (pm-file (buffer-file-name))
              (pm-location (file-name-directory pm-file))
              (package-name (perlnow-get-package-name-from-module-buffer))
              (inc-spot (perlnow-get-inc-spot package-name pm-location))
              )
        (setq perlnow-perl-package-name package-name) ; global to pass value to template
        (perlnow-open-file-other-window
           testfile
           30
           perlnow-perl-test-module-template )
        (save-buffer)
        (funcall (perlnow-lookup-preferred-perl-mode))
        (if new-file-p
            (save-excursion
              (let ( (whitespace
                       (perlnow-jump-to-use package-name) )
                    )
                 (perlnow-endow-script-with-access-to inc-spot whitespace))))
        ))
     ; if script
     ((perlnow-script-p)
      (setq perlnow-perl-script-name (buffer-file-name)) ; global to pass value to template
      (perlnow-open-file-other-window
         testfile
         30
         perlnow-perl-test-script-template)
      (funcall (perlnow-lookup-preferred-perl-mode))
      (save-buffer))
     (t
      (let ((extension (perlnow-file-extension (buffer-file-name))))
        (cond ((string= extension "t")
               (message "Perlnow error: You're already inside of a test file.")
                ;;; TODO - You mean, you can't create a test file for a test file?
                ;;;   Before writing a test, I *always* write a test for it first!
               )
              (t
               (message "This doesn't look like a perl buffer. Perlnow can't edit it's test file.")
               )))))
    (setq perlnow-associated-code original-code)))


(defun perlnow-jump-to-use (package-name)
  "Given the PACKAGE-NAME, jumps to the point before the \'use\' line.
Specifically, these leaves the cursor at the start of the line
that does a \"use\" or \"use_ok\" of the module given in perl's
double-colon seperated form, e.g. \"Modular::Stuff\"."
  (let ( ( pattern (format "^\\([ \t]*\\)use.*?\\b%s\\b" package-name) )
         ( whitespace "" )
         )
    ;; (message "pattern: %s " pattern);; DEBUG
    (goto-char (point-min))
    (re-search-forward pattern nil t)
    (setq whitespace (match-string 1))
    (move-beginning-of-line 1)
    return whitespace))

;; TODO
;; This ends up with a doubled display if
;; the buffer is *already* displayed.  Would be
;; better to switch windows if it there's an already
;; active window.
(defun perlnow-back-to-code ()
  "Return to the code that this testfile is for.
Experimental feature.  Functionality may change."
  (interactive)
; Uses buffer-local variable:
;    perlnow-associated-code
; set by perlnow-edit-test-file, etc.
  (find-file perlnow-associated-code))


;;;==========================================================
;;; Internally used functions
;;;==========================================================


(defun perlnow-file-extension (filename)
  "Returns the file extension of the given FILENAME.
\(I bet one of these has never been written before, eh?\)"
  (let (just_file_name basename extension)
     (setq just_file_name
           (file-name-sans-versions
            (file-name-nondirectory
             filename)))
     (setq basename (file-name-sans-extension just_file_name))
     (setq extension (substring just_file_name (+ 1 (length basename))))))

(defun perlnow-nth-file-path-level (level location)
  "Return the LEVEL indicated from the given file-path.
Usage examples:

    (perlnow-nth-file-path-level 0 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"home\"
    (perlnow-nth-file-path-level 2 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"End\"
    (perlnow-nth-file-path-level 1 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"doom\"
    (perlnow-nth-file-path-level 4 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"Trial\"
    (perlnow-nth-file-path-level 6 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"Liar.pm\"

    (perlnow-nth-file-path-level -1 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"Liar.pm\"
    (perlnow-nth-file-path-level -2 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"Mod\"
    (perlnow-nth-file-path-level -3 \"$HOME/End/Cave/Trial/Mod/Liar.pm\") ;; \"Trial\"

"
  (setq location (perlnow-fixdir location))
  (let* (
        (slash (convert-standard-filename "/"))
        (list (split-string location slash))
        (retval)
        )
    ;; trim leading & trailing blank items
    (if (string= (car list) "")
        (pop list)
        )
    (if (string= (car (last list)) "")
        (setq list (butlast list))
        )
    (cond ( (>= level 0)
            (setq retval (nth level list)))
          ( (< level 0)
            (setq level (+ 1 level))
            (setq retval (nth (abs level) (reverse list))))
          )
    ))


;;;TODO
;;; The following functions:
;;;    perlnow-open-file-other-window
;;;    perlnow-show-buffer-other-window
;;; exist as centralized locations for my current crude window management methods,
;;; so that they can be improved at a later date.  Currently when I want
;;; to show a new window alongside the existing current window, I close
;;; all others and just display the two of them.  Would be better to use
;;; smarter handling that would leave others open if there's enough room.
;;; Question: could both functions be fused together?


(defun perlnow-open-file-other-window (file &optional numblines template switchback)
  "Utility to open file in another window, leaving current
visible.  Options: NUMBLINES, the number of lines in the new
window (defaults to half of frame height); TEMPLATE a
template.el template to be used in creating a new file
buffer.  If SWITCHBACK is true, the cursor is left in the
original window, not the new one."
;;; TODO -
;;; Inelegant interface: *requires* NUMBLINES if you want to feed it a TEMPLATE
  ; before you open, point at where you're going to be from here
  (setq perlnow-associated-code file)    ; bufloc, used by "C-c/b"
  ; and save name of what we're looking at
  (setq original-file-displayed (buffer-file-name)) ; Doesn't work if just a buffer without file...
  (unless numblines
    (setq numblines (- (/ (screen-height) 2) 1) )) ; new window defaults to half of frame height
  (delete-other-windows)
  (setq numblines (- numblines))
  (split-window-vertically numblines) ; Number of lines to display
  (other-window 1)
  (let ((location (file-name-directory file)))
    ;;; TODO consider moving global variable set for template pass biz down here?
    ;;; (probably not workable without dorking out the calling interface)
    (cond ((file-exists-p file)
           (find-file file))
          (t ; file does not exist yet
             ; create directory if need be
           (unless (file-exists-p location)
             (make-directory location t))
           (if template
               (perlnow-create-with-template file template)
             (find-file file))))
    ; after opening, point back from new place to where we were
    (setq perlnow-associated-code original-file-displayed) ; bufloc, used by "C-c/b"
    (if switchback
        (other-window 1))
    ))


(defun perlnow-show-buffer-other-window (buffer &optional numblines switchback)
  "Utility to open BUFFER in another window, leaving current
visible.  Options: NUMBLINES, the number number of lines in
the new window, defaults to half window height; TEMPLATE a
template.el template to be used in creating a new file
buffer.  If SWITCHBACK is true, the cursor is left in the
original window, not the new one. BUFFER can be a string or
a buffer object."
  (unless numblines
    (setq numblines (/ (screen-height) 2) )) ; new window defaults to half of frame height
  (delete-other-windows)
  (split-window-vertically numblines) ; Number of lines to display
  (other-window 1)
  (switch-to-buffer buffer)
  (if switchback
      (other-window 1))
    )

(defun perlnow-do-script (filename)
  "Quickly jump into development of a new perl script.
Prompts the user for the FILENAME.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script]."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (require 'template)
  (perlnow-create-with-template filename perlnow-perl-script-template)
  (perlnow-change-mode-to-executable))



(defun perlnow-do-script-from-module (script-name package-name &optional inc-spot)
  "Does the work of creating a script from a module-buffer.
Takes arguments SCRIPT-NAME PACKAGE-NAME INC-SPOT,
which are all explained in `perlnow-documentation-terminology'.
If INC-SPOT is nil, it skips adding the FindBin/use lib lines.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script] (or possibly the older
\\[perlnow-script-using-this-module]).
Currently always returns t, but future versions may return nil for failure."
; Presumption: if inc-spot is nil, then we got here from a man page buffer,
; and we can assume the module is installed (or the man page most
; likely wouldn't be there).
;
;;; TODO - would be a good idea to check if we can find the
;;;        module and (perhaps) warn if not.
;
    ; Make the script we're creating the default runstring for this module.
    (setq perlnow-module-run-string (format "perl %s" script-name))
    (perlnow-sub-name-to-kill-ring)
    ; module currently displayed, now want to open script, display in paralel
      (perlnow-open-file-other-window
         script-name
         nil
         perlnow-perl-script-template)
      ; forget about a "use" line for things that don't look much perl modules.
      (let ( (case-fold-search nil)
             (import-string "" )      )
        (if (string-match "^[A-Z]" package-name)
            (progn
              (unless (eq inc-spot nil)
                (perlnow-endow-script-with-access-to inc-spot)

                ;;; TODO generate an appropriate "use qw()" import list
                (setq import-string (perlnow-generate-import-list package-name inc-spot))

                )
              ; insert the "use Modular::Stuff;" line
              (insert (format "use %s%s;" package-name import-string))
              (insert "\n")
              )))
      t)

(defun perlnow-endow-script-with-access-to (location &optional whitespace)
  "Insert appropriate \"use lib\" line so script will see given LOCATION."
  (unless (perlnow-inc-spot-in-INC-p location)
    (let* ((script-name (buffer-file-name))
           (relative-path
             (file-relative-name location (file-name-directory script-name))))
      (unless (> (length whitespace) 0) ;; Default to empty string (TODO better way?)
        (setq whitespace ""))
      (insert (format "%suse FindBin qw\($Bin\);\n" whitespace))
      (insert (format "%suse lib \(\"$Bin/" whitespace))
      (insert relative-path)
      (insert "\");\n"))))

;;(setq import-string (perlnow-generate-import-list package-name inc-spot))
(defun perlnow-generate-import-list (package-name inc-spot)
  "Get the default import list from the module PACKAGE-NAME.
Should look something like ' qw( routine1 routine2 routine3 )', where
the three routines are exported (optionally or not) from the indicated module.
If this is not appropriate (e.g. if it's an OOP module, not Exporter based)
this will return the empty string."

  "" ;; stub
     ;; TODO NEXT -- ideally would like to drop inc-spot, and support
     ;; exporter based modules of all sorts, even when creating script from man page.
     ;; Can I easily find code on the system given just the package name?
  )


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



(defun perlnow-prompt-for-cpan-style (where what)
  "For Internal use only: ask the two questions needed for cpan-style dev.
Ala \\[perlnow-h2xs] or  \\[perlnow-module-starter].
The WHERE is the dev location to put the h2xs or module-starter
structure and the WHAT is the name of the perl module to create.
Checks to see if one exists already, and if so, asks for another
name (by doing yet another \\[call-interactively] of another function).
The location defaults to the current `default-directory'.  Returns a
two element list, dev-location and package-name."
  (interactive "DLocation for new module development? \nsName of new module \(e.g. New::Module\)? ")
  (let ( staging-area
         )
  (setq staging-area (perlnow-staging-area where what))
  (while (file-exists-p staging-area)  ; really, directory exists
    (setq where-and-what  ; that's a list: (dev-location package-name)
      (call-interactively 'perlnow-prompt-for-cpan-style-again))
    (setq where (car where-and-what))
    (setq what (cadr where-and-what))
    (setq staging-area (perlnow-staging-area where what))
    )
  (list where what)))


(defun perlnow-prompt-for-cpan-style-again (where what)
  "For internal use only: the \"ask again\" for \\[perlnow-h2xs\].
If the user enters an existing h2xs module name in
\\[perlnow-prompt-for-cpan-style], it will do another chained \\[call-interactively]
to this function to ask again for WHERE and WHAT with a slightly
different message.  Returns a two element list, location and package-name."
  (interactive
   (concat
    "DThat exists already! Location for new module development? \n"
    "sName of new module \(e.g. New::Module\)? " ))
  (list where what))


(defun perlnow-sub-name-to-kill-ring ()
  "Pushes the name of the current perl sub on to the `kill-ring'.
This is intended to be run inside an open buffer of perl code.
It tries to find the name of the current perl sub \(the one that
the cursor is inside of\) and pushes it onto the kill-ring, ready
to be yanked later.  Returns nil on failure, sub name on success.
Used by \\[perlnow-script-using-this-module]."
  (interactive)
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


(defun perlnow-module-found-in-INC (package-name)
  "Given a perl PACKAGE-NAME \(in double-colon separated form\)
return the first module file location found in perl's @INC
array, or nil if it is not found."
  (let* (  full return
           (module-file-tail
            (concat (replace-regexp-in-string "::" perlnow-slash package-name) ".pm"))
           (perl-inc
            (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_\t\"}'" ))
           (inc-path-list (split-string perl-inc "\t"))
           )
    (setq return
     (catch 'TANTRUM
       (dolist (inc-path inc-path-list)
         (setq full (concat (perlnow-fixdir inc-path) module-file-tail))
         (if (file-exists-p full)
             (throw 'TANTRUM full)))))
    return))


(defun perlnow-insert-spaces-the-length-of-this-string (string)
  "Insert as many spaces as characters in the given STRING.
Used by the template.el expansion PNFS."
  (insert
   (make-string (length
                 (file-name-nondirectory string)
                 ) ?\ )))


(defun perlnow-full-path-to-module (inc-spot package-name)
  "Piece together a INC-SPOT and a PACKAGE-NAME into a full file name.
Given \"/home/doom/lib\" and the perl-style \"Text::Gibberish\" would
yield /home/doom/lib/Text/Gibberish.pm or in other words, the
filesys path."
  (let ((filename
         (concat
          (mapconcat 'identity (split-string package-name "::") perlnow-slash)
          ".pm")))
  (setq inc-spot (file-name-as-directory inc-spot))
  (concat  inc-spot filename)))


(defun perlnow-make-sure-file-exists ()
  "Forcibly save the current buffer to it's associated file.
This is to make sure that the file actually exists."
  (set-buffer-modified-p t)
  (save-buffer))


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



(defun perlnow-create-with-template (filename template)
  "Create a new file with a template.el template.
Given FILENAME and TEMPLATE this does the actual creation of
the file and associated buffer using the template.  As a
side-effect, it sets the global `template-file' here."
; The "template-file" must be set here because of a bug in
; template.el, when using template-new-file non-interactively.
  (setq template-file (template-split-filename filename))
  (template-new-file filename template)
  (write-file filename))


(defun perlnow-nix-script-p ()
  "Determine if the buffer looks like a 'nix style executable script.
Looks for the hash-bang line at the top."
  (save-excursion
  (let ( (hash-bang-line-pat "^[ \t]*#!") )
    (goto-char (point-min))
    (looking-at hash-bang-line-pat)
    )))


(defun perlnow-script-p ()
  "Determine if the buffer looks like a perl script.
Looks for the hash-bang line at the top.  Note: this is probably not
a reliable test, since some perl scripts will not have a hash-bang line,
e.g. test files \(*.t\) or scripts on non-unix-like systems."
  (save-excursion
  (let ( (hash-bang-line-pat "^[ \t]*#!.*perl\\b") ) ; note, presumes an explicit "perl"
    (goto-char (point-min))
    (looking-at hash-bang-line-pat))))


(defun perlnow-module-code-p ()
  "Determine if the buffer looks like a perl module.
This looks for the package line near the top."
  (save-excursion
  (let ( (package-line-pat "^[ \t]*package\\b")
         (comment-line-pat "^[ \t]*$\\|^[ \t]*#") )
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (looking-at package-line-pat) )))


(defun perlnow-cpan-style-code-p ()
  "Determine if this file looks like it's in a cpan-style dev tree."
  (save-excursion
    ;;  (0) is this a module?
    ;;  (1) is the module root named "lib"?
    ;;  (2) does the level above match the hyphenized form of module name?
    ;;  (3) is there a "t" in parallel to "lib"
    ;;  (4) is there a MANIFEST there?
    ;;  extra credit:  *.PL file there?  (not done)
    (let* ( (cpan-style-p nil)
            (package-name (perlnow-get-package-name-from-module-buffer))
            (module-file-location
             (file-name-directory (buffer-file-name)))
            (inc-spot
             (perlnow-get-inc-spot package-name module-file-location ))
            (staging-area
             (perlnow-one-up inc-spot))
            (hyphenized-package-name
             (mapconcat 'identity (split-string package-name "::") "-"))
;;            (pm-basename
;;             (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           )
      (setq cpan-style-p
            (and
             (perlnow-module-code-p)   ;; good idea? TODO
             (string= (perlnow-nth-file-path-level -1 inc-spot) "lib")
             (string= (perlnow-nth-file-path-level -2 inc-spot) hyphenized-package-name)
             (file-exists-p    (concat staging-area "MANIFEST"))
             (file-directory-p (concat staging-area "t"))
             )
            )
      cpan-style-p)))

(defun perlnow-get-package-name-from-module-buffer ()
  "Get the module name from the package line.
This will be in perl's double colon separated form, or it will
return nil if none is found."
  (save-excursion
  (let ((package-line-pat "^[ \t]*package[ \t]+\\(.*?\\)[ \t;]") ;; captures "Module::Name"
        (comment-line-pat "^[ \t]*$\\|^[ \t]*#")
         return)
    (goto-char (point-min))
    (while (looking-at comment-line-pat) (forward-line 1))
    (if (looking-at package-line-pat)
        (setq return (match-string 1))
      (setq return nil))
    (set-text-properties 0 (length return) nil return) ; remove all text properties
    return)))


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


(defun perlnow-get-package-name-from-man ()
  "Return the module name from a man page buffer displaying the perldoc.
If not a man page buffer, returns nil.  It tries several methods of
scraping the module name from the man page buffer, and returns
it's best guess."
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
         )
       (t
        (setq return nil))))))


(defun perlnow-vote-on-candidates (candidate-list)
  "Pick the most commonly occuring string from a list of strings.
The list should be given as the argument CANDIDATE-LIST,
the return value will be the string itself.  In the event of a tie
this favors the earlier occurrence in the list."
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


;; TODO really, this is all you need, though, right?
;;   (setq location (perlnow-fixdir "location/.."))
(defun perlnow-one-up (location)
  "Get an absolute path to the location one above the given LOCATION."
  (setq location (perlnow-fixdir location))
  (let ((return
                 (mapconcat 'identity
                            (butlast
                             (split-string location perlnow-slash)
                             2)
                            perlnow-slash)))
    (setq return (perlnow-fixdir return))
    return))
;; DEBUG
;; (perlnow-one-up "/home/doom/tmp/Whatever/")
;; (perlnow-one-up "/home/doom/tmp/Whatever")

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

(defun perlnow-guess-module-run-string ()
  "Return a good guess for an appropriate `perlnow-module-run-string'.
First looks for the Makefile.PL or Build.PL of a cpan-style
distribution.  Failing that this looks for a nearby test file of an
appropriate name.  For example if the module were named
New::Module, the test file could be New-Module.t or Module.t.
The code searches the paths in `perlnow-test-path', which uses a familiar
dot notation \(\".\" \"..\"\) to specify locations relative to either
the module-file-location or the inc-spot.
See: `perlnow-documentation-terminology' and/or
`perlnow-documentation-test-file-strategies'."

  (unless (perlnow-module-code-p)
    (error "This buffer does not look like a perl module (no \"package\" line)"))
  (let* ( (package-name (perlnow-get-package-name-from-module-buffer))
          (module-file-location
            (file-name-directory (buffer-file-name)))
          (inc-spot
            (perlnow-get-inc-spot package-name module-file-location ))
          (hyphenized-package-name
            (mapconcat 'identity (split-string package-name "::") "-"))
          (pm-basename
            (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

          staging-area      ; The location of an h2xs-style dev structure
          staging-area-candidate staging-area-candidate-name
          test-search-list  ; A listing of possible absolute locations to look for the test file,
                            ; built up from relative locations in perlnow-test-path
          testloc testfile
          return            ; the returned run string
          )
    ; first, the cpan-style case
    (cond ( (setq staging-area (perlnow-find-cpan-style-staging-area))
            (cond ( (file-exists-p (concat staging-area "Build.PL"))
                    (setq return (concat "cd " staging-area "; ./Build test"))
                    )
                  ( (file-exists-p (concat staging-area "Makefile.PL"))
                    (setq return (concat "cd " staging-area "; make test"))
                    )
                  ))
          (t ; non-cpan-style module
           (setq testfile (perlnow-get-test-file-name))
           (setq return (format "perl '%s'" testfile))

            ))
    return))

(defun perlnow-american-date ()
  "Return the date in the common American format: MM/DD/YY.
Much derided though it may be.  Note: no leading zeros on MM or DD."
  (let* (
         (tl (decode-time (current-time)))
         (day   (nth 3 tl))
         (month (nth 4 tl))
         (year  (nth 5 tl))
         (year-str (format "%d" year))
         (year2 (substring year-str 2))
         (merkin-date (format "%d/%d/%s" month day year2))
       )
    merkin-date))

(defun perlnow-full-date ()
  "Return the date in the fully-spelled out American format.
For example: \"August 8, 2009\" (which I realize is not *just* American)."
;;  (interactive) ;; DEBUG
  (let* (
         (month (format-time-string "%B"))
         (day   (format "%d" (nth 3 (decode-time (current-time)))))
         (year (format-time-string "%Y"))
         (fulldate (format "%s %s, %s" month day year))
       )
    fulldate
;;    (message fulldate)
  ))

;;;==========================================================
;;; The following functions are used by perlnow-edit-test-file
;;; and it's relatives.
;;;==========================================================

(defun perlnow-get-test-file-name ()
  "Looks for the test file for the current perl code buffer."
   (let (testfile)
   (cond ( (perlnow-cpan-style-code-p)
           (setq testfile (perlnow-get-test-file-name-cpan-style)))
         ( (perlnow-module-code-p)
           (setq testfile (perlnow-get-test-file-name-module)))
         ( (perlnow-script-p)
           (setq testfile (perlnow-get-test-file-name-script)))
         (t
    ;;; TODO
    ;;; ask user first if this is really a perl script?
           (setq testfile (perlnow-get-test-file-name-script))))
   testfile))


(defun perlnow-get-test-file-name-module ()
   "Get the test file name for the current perl module buffer.
Used by \\[perlnow-get-test-file-name]."
  (perlnow-get-test-file-name-given-policy
    perlnow-test-policy-test-location
    perlnow-test-policy-dot-definition
    perlnow-test-policy-naming-style))

(defun perlnow-get-test-file-name-cpan-style ()
  "Get the test file name for the current perl module buffer.
  Used by \\[perlnow-get-test-file-name]."
  (perlnow-get-test-file-name-given-policy
   "../t"     ;; perlnow-test-policy-test-location
   "incspot"  ;; perlnow-test-policy-dot-definition
   "numeric"  ;; perlnow-test-policy-naming-style
   ))

(defun perlnow-get-test-file-name-script ()
   "Get the test file name for the current perl script buffer.
Used by \\[perlnow-get-test-file-name]."
  (
    perlnow-get-test-file-name-given-policy
    perlnow-test-policy-test-location
    "fileloc"
    "basename"))


(defun perlnow-get-test-file-name-given-policy (testloc dotdef namestyle)
   "Get the test file name for the current perl buffer, given a test policy.
This is used by \\[perlnow-get-test-file-name] and relatives.
A test policy (see `perlnow-documentation-test-file-strategies')
is defined by three pieces of information:
the TESTLOC \(see `perlnow-test-policy-test-location'\)
the DOTDEF \(see `perlnow-test-policy-dot-definition' \)
and the NAMESTYLE \(see `perlnow-test-policy-naming-style'\)."
;;; Note: perlnow-edit-test-file docs explains a lot of what
;;; has to happen here. I quote:
;;   o Checks the test policy, looks for an existing file there.
;;   o If not, then searches the test path, looks for an existing file there
;;   o   (If more than one is found it will complain).
   (let* (
         ; script oriented info:
           (file-location
             (file-name-directory (buffer-file-name)))
           (basename
             (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         ; module oriented info (calculated below):
           (package-name "")
           (inc-spot "")
           (hyphenized-package-name "")
          ; also still need to determine:
           (testloc-absolute "")
           (test-file-from-policy "")
           (test-file "")
           )
    ; module oriented info, calculated:
    (cond ; do only if module
     ((setq package-name (perlnow-get-package-name-from-module-buffer))
           (setq inc-spot (perlnow-get-inc-spot package-name file-location))
           (setq hyphenized-package-name (mapconcat 'identity (split-string package-name "::") "-"))
           ))
     ; define testloc-absolute
     (cond ((string= dotdef "fileloc") ; might be script or module
             (setq testloc-absolute
                 (perlnow-expand-dots-relative-to file-location testloc)))
           ((string= dotdef "incspot") ; only with modules
             (setq testloc-absolute
                 (perlnow-expand-dots-relative-to inc-spot testloc)))
           (t
            (error
             "Invalid perlnow-test-policy-dot-definition setting, should be 'fileloc' or 'incspot'")))
     ; define test-file-from-policy
     (cond ( (string= namestyle "hyphenized")  ; only with modules
               (setq test-file-from-policy
                   (concat testloc-absolute hyphenized-package-name ".t"))
             )

           ( (string= namestyle "numeric")  ; only with modules (? TODO)
               (setq test-file-from-policy
                   (concat testloc-absolute "01-" hyphenized-package-name ".t")) ;; default created by modstar
             )

           (t
            (error
             "Invalid perlnow-test-policy-naming-style setting, must be hyphenized, basename or numeric")))
     ;If this result is good, return it, if not, keep looking
     ;If nothing found though, return this as name to be created.
     (cond ((file-exists-p test-file-from-policy)    ; if test-policy finds test-file, does not look for redundant matches
             (setq test-file test-file-from-policy) )
           ((setq test-file (perlnow-search-through-test-path)) ) ; warns if redundant matches exist,
                                                                  ; but returns the first.  nil if none.
           (t
              (setq test-file test-file-from-policy))
           )
     test-file))

;;; TODO SOON
;;; Not yet in use.
;;; TODO check if this is limited to cpan-style
;;; TODO somewhere need to be able to do recursive decent through a project tree
;;; TODO
;;; Display a buffer of all associated test files, allow choice between them.
(defun perlnow-list-test-files (testloc dotdef namestyle &optional choose-one)
  "Looks for test files associated wtih the current file.
Uses the three given elements of a \"test policy\", to find
associated test files:
A test policy (see `perlnow-documentation-test-file-strategies')
is defined by three pieces of information:
the TESTLOC \(see `perlnow-test-policy-test-location'\)
the DOTDEF \(see `perlnow-test-policy-dot-definition' \)
and the NAMESTYLE \(see `perlnow-test-policy-naming-style'\).
Returns a list of appropriate test files found, or just a best
pick if the CHOOSE-ONE option is non-nil."
;;; Note, code mutated from above: perlnow-get-test-file-name-given-policy
   (let* (
         ; script oriented info:
           (file-location
             (file-name-directory (buffer-file-name)))
           (basename
             (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         ; module oriented info (calculated below):
           (package-name "")
           (inc-spot "")
           (hyphenized-package-name "")
          ; also will determine:
           (testloc-absolute "")
           (test-file-from-policy "")
           (test-file "")
           (test-file-list ())
           )
    ; module oriented info, calculated:
    (cond ; do only if module
     ((setq package-name (perlnow-get-package-name-from-module-buffer))
           (setq inc-spot (perlnow-get-inc-spot package-name file-location))
           (setq hyphenized-package-name (mapconcat 'identity (split-string package-name "::") "-"))
           ))
     ; define testloc-absolute
     (cond ((string= dotdef "fileloc") ; might be script or module
             (setq testloc-absolute
                 (perlnow-expand-dots-relative-to file-location testloc)))
           ((string= dotdef "incspot") ; only with modules
             (setq testloc-absolute
                 (perlnow-expand-dots-relative-to inc-spot testloc)))

           (t
            (error
             "Invalid perlnow-test-policy-dot-definition, should be fileloc, incspot or parallel")))

     (setq testloc-absolute (perlnow-fixdir testloc-absolute))

     (unless (file-directory-p testloc-absolute)
       (message "warning %s is not a directory" testloc-absolute))

     (setq test-file-list
           (directory-files testloc-absolute nil "\.t$"))

     (if (choose-one)
         (setq test-file-list (list (perlnow-latest-test-file test-file-list)))
       )
     test-file-list))

;; for DEBUG
(defun perlnow-test-files-report ()
  "Report the test files associated with current buffer.
Currently limited to cpan-style code buffers (hardcoded params)."
  (interactive)
  (let* ( (listsky
           (perlnow-list-test-files "../t" "incspot" "numbered"))
          (return (mapconcat 'identity listsky " "))
          )
    (message "%s" return)
  ))


(defun perlnow-latest-test-file (test-file-list)
  "Given a list of test files, select the \"latest\" one.
By latest, we mean the one a developer is most likely to want
to work on."
;; first cut:
;; grep for numeric prefixes, sort, return the last.
  (let* ( (new-list
            (perlnow-grep "^[0-9]*?-" test-file-list))
          (new-list (sort new-list 'string<))
          (last-item (car (last new-list)))
          )
;;    new-list ;; intermediate return for DEBUG
    last-item
    ))

(defun perlnow-grep (pattern list)
  "A (probably naive) implementation of perl's grep.
Return a new list of elements from LIST that match PATTERN.
LIST is presumed to be a list of strings."
  (let ( (new-list) )
    (dolist (item list)
      (if (string-match pattern item)
           (setq new-list (cons item new-list))
        ))
    new-list))

;; TODO re-write to find "latest numeric"? -- Mon Sep  7 13:38:35 2009
;; Possibly using: perlnow-latest-test-file
(defun perlnow-search-through-test-path ()
  "Searches the test path for test files for the current code buffer.
Returns a single string the full-path and name of (one) test file found.
Will warn if there appear to be redundant possible testfiles."
;;; *Might* be better to return a list of all matches, let other
;;; code check for and complain about the problem of multiple finds.
  (let*  (
           (test-search-list ())  ; A listing of possible absolute locations to look for the test file,
                                  ; built up from relative locations in perlnow-test-path
           testloc   ; a location to be searched for test files
           testfile  ; a possible testfile to check for existance

           fish-list ; list of "catches" that look like appropriate test files
           return            ; the returned run string

           file-location
           basename
           package-name
           inc-spot
           hyphenized-package-name
           test-file-check-list
           )
      ;;; This block of code was c&p from above, and ported outside of
      ;;; the let* to allow for cond usage.  Fugliness, eh?
      ;;; TODO - routine that probes for all possible info like this
      ;;; you could want, and stashes it in a data structure like an alist
      ;;; which you can then pass around if you like.
           ; script oriented info:
           (setq file-location
             (file-name-directory (buffer-file-name)))
           (setq basename
             (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
          ; module oriented info:
           (cond ( (setq package-name (perlnow-get-package-name-from-module-buffer))
                   (setq inc-spot (perlnow-get-inc-spot package-name file-location))
                   (setq hyphenized-package-name
                     (mapconcat 'identity (split-string package-name "::") "-"))
                   ))
       ;;; TODO - Consider exposing this list to users in some form,
       ;;;        via a defvar or something
           ; This is a listing of possible names for the test file:
           (setq test-file-check-list (list
                                       (concat hyphenized-package-name ".t")
                                       (concat "01-" hyphenized-package-name ".t")
                                       (concat basename ".t")
                                       ))
   ;;; TODO NOW
   ;;; Question: is the following general code that would work on a script *or* a module file?
    ; load test-search-list:
    ;    do munging of dots, deal with different possible meanings of "here"
    (dolist (testloc-dotform perlnow-test-path)
      (setq testloc
            (perlnow-expand-dots-relative-to file-location testloc-dotform))
      (if (file-directory-p testloc)
          (setq test-search-list (cons testloc test-search-list)))
      (cond (inc-spot ; don't bother with followin if not a module with defined inc-spot
             (setq testloc
                   (perlnow-expand-dots-relative-to inc-spot testloc-dotform))
             (if (file-directory-p testloc)
                 (setq test-search-list (cons testloc test-search-list)))
             )))
    ; tracking down the *.t files (if any)
    (dolist (real-place test-search-list)
      (dolist (possible-name test-file-check-list)
        (setq testfile
              (concat
               (perlnow-fixdir real-place) ;; I bet this fixdir is redundant
               possible-name))
        (if (file-regular-p testfile)
            (setq fish-list (cons testfile fish-list)))))
    ; handle the case of multiple possible test files
    (cond ((> (length fish-list) 1)
           (let ( (i 1)
                  (warning "PERLNOW WARNING: more than one valid test file (using the first):"))
             (dolist (fish fish-list )
               (setq warning (concat warning (format "%d: %s\t" i fish)))
               (1+ i))
             (message warning)
             (setq return (nth 0 fish-list)))) ; return first for the hell of it
          ((= (length fish-list) 1)
           (setq return (car fish-list)))      ; return the only one
          ((= (length fish-list) 0)
           (setq return nil))                  ; return nil if we got none
          (t
           (message "List appears to have negative length. Huh?")
           ))
    ))


(defun perlnow-assoc-regexp (pattern alist &optional default)
  "Return first value from ALIST with key that matches PATTERN."
    (assoc-default pattern alist 'string-match default))


(defun perlnow-lookup-preferred-perl-mode ()
  "Look-up which perl mode the user prefers.
Examines the alists `interpreter-mode-alist' and
`auto-mode-alist' to see if perl-mode,
cperl-mode \(or perhaps something else entirely?\)
has been chosen as the default to work on perl code."
  (interactive)
  (let* ( (default "cperl-mode")
          (mode default)
          (interpreter-rule "perl") ; should match perl or perl5
          (auto-rule "\[[pP][pP]\]\[[Llm][Llm][Llm]\]") ; regexp to match a regexp containing: [pP][Llm]
         )
      (cond ((setq mode
              (perlnow-assoc-regexp interpreter-rule interpreter-mode-alist default))
             )
            ((setq mode
              (perlnow-assoc-regexp auto-rule auto-mode-alist default))
              )
            (t
             (setq mode default)))
      mode))

;;;==========================================================
;;; The end of perlnow-edit-test-file family of functions
;;;==========================================================


(defun perlnow-guess-script-run-string ()
  "Return a good guess for `perlnow-script-run-string'.
Also sets that global variable as a side-effect."
;;; Presumption is that this won't be called if we're in a module,
;;; so there's no point in testing that again.
  (let ( (perl-command)
         (run-line)
         (staging-area)
         (filename (buffer-file-name))
         )
  ;;# check for hash bang:
  (cond ( (setq perl-command (perlnow-hashbang))
           ; preserve the hash-bang run string, e.g. to preserve -T
          (setq run-line (concat perl-command " " filename))
           )
        ( (string-match "\.t$"  filename) ; it's a test file
          (cond ( (setq staging-area (perlnow-find-cpan-style-staging-area))
                  (cond ( (file-exists-p (concat staging-area "Build.PL"))
                          (setq run-line (concat "cd " staging-area "; ./Build test"))
                          )
                        ( (file-exists-p (concat staging-area "Makefile.PL"))
                          (setq run-line (concat "cd " staging-area "; make test"))
                          )
                        ))
                (t ; non-cpan-style code
                 (setq run-line (format "perl %s" filename))
                 )
             )
          )
        (t ; When all else fails, just feed it to perl and hope for the best
         (setq run-line (format "perl %s" filename))
          ))
  (setq perlnow-script-run-string run-line)
  run-line))


(defun perlnow-find-cpan-style-staging-area ()
  "Determines if the current file buffer is located in an h2xs tree.
Should return the path to the current h2xs staging area, or nil
if it's not found.  The staging area is located by searching upwards
from the location of the buffer's associated file for a place
with a \"lib\" and/or \"t\" *and* a \"Makefile.PL\"."
;; Two cases I definitely want to cover:
;;   ~/perldev/Horror-Grossout/lib/Horror/Grossout.pm
;;   ~/perldev/Horror-Grossout/t/Horror-Grossout.t
;;
;; This uses a simple method:
;; Crawl up from file location, until "t" and/or "lib" is found.
;; Is there a Makefile.PL next to them?

  (let* ( ; args for directory-files function:
          dir       ; candidate directory under examination
          (full-names nil)
          (pattern "^[ltMB]") ; pre-screen listing for interesting results only
          (nosort t)

          file-list ; file listing of the candidate directory (pre-screened)
          return)
    (setq dir (perlnow-fixdir (file-name-directory (buffer-file-name))))
    (setq return
          (catch 'ICE
            (while (> (length dir) 1)
              (setq file-list (directory-files dir full-names pattern nosort))
              (dolist (file file-list)
                (if (or (string= file "lib") (string= file "t")) ; we're here!
                    ; start scan again: "Makefile.PL" might be before or after lib or t
                    (dolist (file file-list)
                      (if (or (string= file "Makefile.PL") (string= file "Build.PL")) ; we found it!
                          (throw 'ICE dir)))))
              (setq dir (perlnow-one-up dir)))
            (setq return nil))) ; ran the gauntlet without success, so return nil
    (if return ; skip if nothing found (and dir is "/").
        (perlnow-cpan-style-build dir))
    return))


;; replaces perlnow-run-perl-makefile-pl-if-needed & perlnow-run-perl-build-pl
;; TODO -- should this bring the display-buffer up front?
(defun perlnow-cpan-style-build (staging-area)
  "Does the cpan-style build in the STAGING-AREA (but only if needed).
Specifically, this runs Makefile.PL and/or Build.PL.
Output is appended to the *perlnow-build* window."
;; Note: relies on naming convention, "perl *.PL" creates target "*".
  (let* ( (display-buffer-name "*perlnow-build*")
          (display-buffer)
          (builders (list "Makefile.PL" "Build.PL"))
          (return-flag nil)
          )
    (dolist (builder builders)
      (let* (
            (build-target (file-name-sans-extension builder))
            (build-target-full (concat staging-area build-target))
            (builder-full    (concat staging-area builder))
            )

      (cond ( (not (file-newer-than-file-p build-target-full builder-full))
              (setq display-buffer (get-buffer-create display-buffer-name))
              (set-buffer display-buffer)

              (cond ( (file-regular-p builder-full)
                      (insert (format "Perlnow is generating %s from %s, in %s...\n"
                                      build-target builder staging-area))
                      (let ( (default-directory staging-area) )
                        (call-process "perl"
                                      nil
                                      display-buffer
                                      nil
                                      builder
                                      ))
                      (end-of-buffer display-buffer)
                      ))))

      (if (file-regular-p build-target-full)
          (setq return-flag t))
      ))
    return-flag
    ))

;; TODO DELETE SOON
;;; The Module::Build analog of: perlnow-run-perl-makefile-pl-if-needed
(defun perlnow-run-perl-build-pl (staging-area)
  "Given a STAGING-AREA in an h2xs tree, runs \"perl Build.PL\" if needed.
Looks to see if there's a file named Build there, and if not,
runs the \"perl Build.PL\" command to generate it.
Output is appended to the *perlnow-build* window."

;;; Note, this *presumes* that you're inside an staging-area, it does not check.
;;; TODO should really compare age of Build vs Build.PL
  (let (display-buffer )
    (cond ( (not (file-regular-p (concat staging-area "Build")))
            (setq display-buffer (get-buffer-create "*perlnow-build*"))
            (set-buffer display-buffer)
            (insert "Trying to generate Build from Build.PL\n")
            (let ( (default-directory staging-area) )
              (call-process "perl"
                            nil
                            display-buffer
                            nil
                            "Build.PL"
                            ))))))



;; TODO DELETE SOON
(defun perlnow-run-perl-makefile-pl-if-needed (h2xs-staging-area)
  "Given a H2XS-STAGING-AREA in an h2xs tree, runs \"perl Makefile.PL\" if needed.
This looks to see if there's a Makefile there, and if not,
runs the \"perl Makefile.PL\" command to generate it.
Output is appended to the *perlnow-h2xs* window."
;;; Note, this *presumes* that you're inside an h2xs-staging-area, it does not check.
;;; TODO should really compare age of Makefile vs Makefile.PL
  (let (display-buffer )
    (cond ( (not (file-regular-p (concat h2xs-staging-area "Makefile")))
            (setq display-buffer (get-buffer-create "*perlnow-h2xs*"))
            (set-buffer display-buffer)
            (insert "Trying to generate Makefile from Makefile.PL\n")
            (let ( (default-directory h2xs-staging-area) )
              (call-process "perl"
                            nil
                            display-buffer
                            nil
                            "Makefile.PL"
                            ))))))


(defun perlnow-hashbang ()
  "What is the hash bang line for this file buffer?
Returns nil if there is none."
  (save-excursion
    (let ( (hash-bang-pat (concat     ; Want:  "^#!(rest captured)"
                           "^"
                           "[ \t]*"   ; Allowing whitespace between everything
                           "#"
                           "[ \t]*"
                           "!"
                           "[ \t]*"
                           "\\(.*\\)$"
                           ))
           (return "")
           )
      (goto-char (point-min)) ; Presume the hash bang, if any, is the first line (no blanks or comments)
      (looking-at hash-bang-pat) ; why not just string-match?
      (setq return
            (match-string 1))
      )))

(defun perlnow-get-inc-spot (package-name pm-location)
  "Determine the module root, the place where the package namespace begins.
Given the PACKAGE-NAME \(e.g. \"New::Module\"\),
and the PM-LOCATION \(as an absolute path to the \".pm\" file,
e.g. for \"/home/doom/perldev/Punk/Skunk/New/Module.pm\"\
the PM-LOCATION is \"/home/doom/perldev/Punk/Skunk/New/\"\),
Returns the module root, \(which in this example is:
\"/home/doom/perldev/Punk/Skunk/\"\) Returns nil if pm-location is nil."
  ;; Example:
  ;;  /home/doom/perldev/Punk/Skunk/New/Module.pm
  ;;  /home/doom/perldev/Punk/Skunk/New/              => number of slashes:  7
  ;;                                New::Module       => double-colon-count: 1
  ;;  /home/doom/perldev/Punk/Skunk/                  The desired inc-spot
  ;;
  (let (( inc-spot ))
    (cond ((eq pm-location nil)
           (setq inc-spot nil))
          (t
           ;; Conditioning pm-location: if there's a trailing .pm, strip the last level
           (if (string-match (concat "^\(.*?" perlnow-slash "\).*?\\.pm$") pm-location)
               (setq pm-location (match-string 1 pm-location)))
           ;; Ensure there's a trailing slash (among other things)
           (setq pm-location (perlnow-fixdir pm-location))

           (let* ( (module-terms-list (split-string package-name "::"))
                   (rev-module-terms-list (reverse module-terms-list))
                   (pattern)
                   )
             (pop rev-module-terms-list) ; discard lowest level (the *.pm)
             (setq inc-spot pm-location) ; will trim terms from end, and return
             (dolist (term rev-module-terms-list)
               (setq pattern (concat "^\\(.*?" perlnow-slash "\\)" term perlnow-slash "$"))
               (if (string-match pattern inc-spot)
                   (setq inc-spot (match-string 1 inc-spot))
                 (error "%s from %s not found in expected place in %s"
                        term package-name pm-location)
                 )
               ))))
          inc-spot))

(defun perlnow-perlversion-old-to-new (given-version)
  "Convert old form of perl version into the new form.
For example, an GIVEN-VERSION might be 5.006 for which the new is 5.6.0
which is more suitable for use as the -b parameter of h2xs.
If given a version that is already in the new style, just
passes it through unchanged."
;; TODO -- the regexps here probably need improvement.
;; Get a definitive list of cases of perl versions that it
;; should handle, write a unit test, and refactor this
  (let ( (old-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)$")
         (new-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9]*\\)\\.\\([0-9][0-9]*\\)")
         major
         mantissa
         minor1)
    (cond
     ( (string-match new-version-pat given-version)
;;       (message "Looks like minimum perl version is in the new style: %s" given-version) ;; DEBUG
        given-version )
     ( (string-match old-version-pat given-version)
       (setq major (match-string 1 given-version))
       (setq mantissa (match-string 2 given-version))
       (setq minor1 (substring mantissa 2))
       (concat major "." minor1 "." "0") )
     (t
       (error "Can not make sense of this perl version: %s" given-version))
     )
    ))


(defun perlnow-staging-area (dev-location package-name)
  "Return path to staging area for DEV-LOCATION & PACKAGE-NAME."
  (let ((staging-area
         (file-name-as-directory
         (concat
          (perlnow-fixdir dev-location)
          (mapconcat 'identity (split-string package-name "::") "-")))))
    staging-area))


(defun perlnow-full-path-to-cpan-style-module (dev-location package-name)
  "Get the full path to a module created by h2xs.
E.g. if the DEV-LOCATION were \"/usr/local/perldev\" and the PACKAGE-NAME
were \"New::Module\", this should return:
\"/usr/local/perldev/New-Module/lib/New/Module.pm\""
  (let ((pm-file
         (concat
          (file-name-as-directory dev-location)
          (mapconcat 'identity (split-string package-name "::") "-")
          "/lib/"
          (mapconcat 'identity (split-string package-name "::") perlnow-slash)
          ".pm")))
    pm-file))


(defun perlnow-full-path-to-dev-test-file (staging-area)
  "Get the full path to a the test file for a module created by h2xs.
Given the STAGING-AREA, it looks for files located in the
sub-directory \"t\".  First choice is given to a test file with
a basename related to the module name, if that fails it looks
for the old-fashioned \"1.t\".  E.g. if the staging-area were
\"/usr/local/perldev/New-Module/\" it would look in
\"/usr/local/perldev/New-Module/t\" for \"New-Module.t\" or
\"Module.t\" or possibly \"1.t\"."
  (let (  (module-test-location "")
          (test-file1 "")     ; new-style, e.g.      New-Module.t
          (test-file2 "")     ; strange beast, e.g.  Module.t
          (test-file3 "1.t")  ; old-style numeric file name
          (test-file "")      ; returned value
          (basename "")
          (basename-truncated "")
          )
    (setq staging-area (perlnow-fixdir staging-area))  ;;; redundant fixdir now?
    (setq module-test-location
            (concat staging-area "t/"))
    ; peel off the lower level of "staging-area",
    ; to get the probable base-name
    (let ((dir staging-area))
;      (string-match "\\(^.*/\\)\\([^/]*\\)[/]*$" dir)
      ;;; TODO - regexp has unix slash dependency
      (string-match "\\(^.*/\\)\\([^/]*\\)/$" dir)
      (setq basename (match-string 2 dir))
      (unless basename
        (message "warning: blank basename found in perlnow-full-path-to-dev-test-file"))
      )
    (setq test-file1 (concat module-test-location basename ".t"))
    ; for the hell of it, peel off the last part
    ; of that name, a second try for basename (not likely)
    (string-match "\\(^.*-\\)\\([^-]*\\)$" basename)
    (setq basename-truncated (match-string 2 basename))
    (setq test-file2 (concat module-test-location basename-truncated ".t"))
   ; And failing that, well try the numeric name, 1.t
   ; And if *that* fails, we'll return the directory location
   ; (a feature that might be better than just returning a
   ; single file, eh?  Maybe should only open the h2xs test file
   ; when there's only one there...  Think about that -- TODO).
   (cond ( (file-exists-p test-file1)
           (setq test-file test-file1 ) )
         ( (file-exists-p test-file2)
           (setq test-file test-file2 ) )
         ( (file-exists-p test-file3)
           (setq test-file test-file3 ) )
         ( (file-directory-p module-test-location)
           (setq test-file module-test-location))  ;; would that work, returning a directory?
         (t
          (error "Can't find h2xs test file or test location")
          ))
    test-file))

(defun perlnow-full-path-new-module-starter-test-file (modstar-location package-name)
  "Get the full path to a the new test file to be added to a
structure created by module_starter (using Module::Build).
Follows a very simple fixed policy, given a module named
Modular::Stuff creates a file called 01-Modular-Stuff.t."
  (let* (
          (hyphenated (mapconcat 'identity (split-string package-name "::") "-"))

          (location (concat (perlnow-fixdir modstar-location)
                            "/"
                            "t"
                            ))

          (filename (format "01-%s.t" hyphenated))

          (fullname (concat location "/" filename))
          )
    fullname))

(defun perlnow-blank-out-display-buffer (buffer &optional switchback)
  "Clear out a temporary display BUFFER.
Erase the contents of a buffer, though only if it matches
the convention for temporary display buffers, i.e. it has
a name beginning with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument BUFFER can be a string or
a buffer object.  This can work on a read-only buffer."
  (let ((original-buff (buffer-name))
        (original-default-directory default-directory)
        original-read-only-status)
  ; Buffer argument may be string or buffer object
  (if (char-or-string-p buffer) ; stringp better ? would a char work?
      (setq buffer (get-buffer-create buffer)))
  (if (not (string= "*" (substring (buffer-name buffer) 0 1)))
      (error "Will not blank out a buffer that does not begin with \"*\""))
  ; clear buffer if it exists, create it otherwise
  (if (buffer-live-p buffer)
      (progn
        (set-buffer buffer)
        (setq original-read-only-status buffer-read-only)
        (setq buffer-read-only nil) ; make sure buffer is writeable
        (mark-whole-buffer)
        (delete-region (mark) (point))
        (setq buffer-read-only original-read-only-status) ; make it read-only if we found it that way
        )
    (get-buffer-create buffer))
  (if switchback
   (set-buffer buffer))
  (setq default-directory original-default-directory)))


(defun perlnow-inc-spot-in-INC-p (&optional inc-spot)
  "Determine if the INC-SPOT has been included in perl's @INC search path.
If not given a INC-SPOT, it defaults to using the module root of the
current file buffer.  Used by \\[perlnow-do-script-from-module]."
; Note: Just checking getenv("PERL5LIB") would be close, but
; using @INC as reported by perl seems more solid, so that's
; what we do here.
  (unless inc-spot
    (setq inc-spot
          (perlnow-get-inc-spot
           (perlnow-get-package-name-from-module-buffer)
           (file-name-directory (buffer-file-name)))))
    (let* (
      (perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_\t\"}'" ))
      (inc-path-list (split-string perl-inc "\t"))
      return )
      (setq return
            (catch 'UP
              (dolist (path inc-path-list)
                (if (string= path inc-spot)
                    (throw 'UP t)))))
      return))
;;; TODO
;;; Consider loading a lisp structure with @INC once early on,
;;; so we won't need to do the above repeatedly

;;;==========================================================
;;; The following code is used by perlnow-module:
;;; perlnow-prompt-for-new-module-in-one-step and relatives
;;; are used to read in perlmodule path and names in one step
;;; (A variant of the old perlnow-prompt-for-module-to-create.)
;;;
;;; Note: instead of completing-read this uses read-from-minibuffer
;;; with a customized keymap that totally transforms it's behavior.
;;;
;;; For a discussion of the following code, see this article:
;;; http://obsidianrook.com/devnotes/elisp-prompt-new-file-part3.html
;;;
;;;==========================================================


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


(defun perlnow-read-minibuffer-complete ()
  "Does automatic completion of up to an entire directory or file name.
Used in reading in path and name of a perl module \(which
need not exist already, though a portion of the file system
path for it may exist, and autocompletion should be
available for the parts that do exist\).  Valid name
separators are \(\"/\" or \"::\"\).\n
This makes no attempt at a more aggressive completion past
a file-system name separator boundary."
;;; codename: new tabby
  (interactive)
  (let ((restrict-to-word-completion nil))
        (perlnow-read-minibuffer-workhorse restrict-to-word-completion)
    ))


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
    ; No single trailing colons allowed: double them up
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
               (setq file-system-path (replace-regexp-in-string "::" perlnow-slash perlish-path))
               ; swap in file system separator "/"  for perl package separators "::"
               (setq separator "::"))
              (t
               (setq separator perlnow-slash)
               (setq file-system-path perlish-path)))
        (setq candidate-alist
              (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
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
        ; (Yeah, checking *again*. Inelegant, but WTH)
        (if (string-match pm-extension-pat suggested-completion)
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
    ))



(defun perlnow-read-minibuffer-completion-help ()
   "Show the available completions when reading in path & name of a module.
Most likely this will be called by \\\[perlnow-read-minibuffer-complete-word]
and \\\[perlnow-read-minibuffer-complete] \(at least indirectly, through
\\\[perlnow-read-minibuffer-workhorse])\), though it's also expected to
be bound to the \"?\" key during the minibuffer read."
;;; codename: huh
  (interactive)
  (let* (
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
         (file-system-path (replace-regexp-in-string "::" perlnow-slash perlish-path) )
            ; unix file system separator "/" swapped in for perl package separators "::"
         match-alist
         )
    (setq match-alist (perlnow-list-directories-and-modules-as-alist file-system-path fragment-pat))
    (setq match-alist (perlnow-remove-pm-extensions-from-alist match-alist))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (all-completions fragment match-alist)
       ))
    ))


(defun perlnow-remove-pm-extensions-from-alist (alist)
  "Remove the pm extension from the names in the ALIST of file names and values.
Currently this throws away the numeric value and re-numbers the names in the
alist in order."
; Does the numbering of items in the alist matter one way or another?
  (let (name new-alist (i (length alist)) )
    (dolist (pair alist)
      (setq name (car pair))
      (setq name (replace-regexp-in-string "\\.pm$" "" name))
      (setq new-alist (cons (cons name i) new-alist))
      (setq i (- i 1))
      )
   (setq new-alist (reverse new-alist))
   ))


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



(defun perlnow-list-directories-as-alist (file-system-path pattern)
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



(defun perlnow-split-perlish-package-name-with-path-to-inc-spot-and-name (string)
  "Split the hybrid form of a module path into the two components.
Input STRING is expected to be a hybrid file system
path using slashes for the module root name space, and
double colons for the package name space inside of that.
This is split into two pieces, the module root
and module name, which are returned as a two-element list."
;;; TODO
;;; Fix any portability problem here.  Can pattern [^/] work on windows?
;;; Why not build it up using perlnow-slash?
  (let* ( (pattern
            (concat
             "^\\(.*\\)"       ; ^(.*)    - stuff at start becomes the mod root
             perlnow-slash     ; /        - the right-most slash, because:
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



(defun perlnow-interesting-file-name-p (string)
  "Is the given file \(or directory name\) be interesting?
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


(defun perlnow-split-module-path-to-dir-and-tail (string)
  "Split a file system path into directory and trailing name fragment.
Allows for the use of perl's double-colon package
name separators in addition to the usual unix-like slash
character.\n
Simple example: given the STRING \"/home/doom/lib/Stri\" should return
 \"/home/doom/lib/\" and \"Stri\"\n
Perl package example: given \"/home/doom/lib/Taxed::Reb\" should return
 \"/home/doom/lib/Taxed::\" and \"Reb\"\n"
;;; TODO - fix unix file separator depencency here
;;;        (build up with perlnow-slash?)
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
;;; Experimental functions
;;;==========================================================

;; Insert boilerplate commands.
;; There might be some reason to use skeleton.el or tempo.el for these.

;; Note: the following presume interspersed pod style.

(defun perlnow-insert-basic-sub (name)
  "Insert the framework of a basic perl sub definition"
  (interactive "sMethod name: ")
  (insert (concat
           "\n"
           "=item " name "\n"
           "\n"
           "=cut" "\n"
           "\n"
           "sub " name " {" "\n"
           "  my $arg = shift;" "\n"
           "\n"
           "\n"
           "}" "\n"
           ))
  (previous-line 3)
  )

;; perl-OOP-oriented:
;; Currently these are limited to hashref-based oop.
;; Need more preference settings, ideally with project specific overrides.

(defun perlnow-insert-method (name)
  "Insert the framework of a perl method definition"
  (interactive "sMethod name: ")
  (insert (concat
           "\n"
           "=item " name "\n"
           "\n"
           "=cut" "\n"
           "\n"
           "sub " name " {" "\n"
           "  my $self = shift;" "\n"
           "\n"
           "\n"
           "}" "\n"
           ))
  (previous-line 3)
  )

(defun perlnow-insert-accessors (field)
  "Insert the basic framework for a perl setter and getter,
Presumes href-based objects.  Uses two variables to define the
naming convention for the accessors: \\[perlnow-getter-prefix],
and \\[perlnow-setter-prefix]."
  (interactive "sObject Attribute name: ")
  (let (
        (getter-prefix perlnow-getter-prefix)
        (setter-prefix perlnow-setter-prefix)
        (getter (concat getter-prefix field))
        (setter (concat setter-prefix field))
        )
  (insert (concat
           "\n"
           "=item " getter "\n"
           "\n"
           "Getter for object attribute " getter "\n"
           "\n"
           "=cut" "\n"
           "\n"
           "sub " getter " {" "\n"
           "  my $self = shift;" "\n"
           "  my $" field " = $self->{ " field " }; " "\n"
           "  return $" field ";"                     "\n"
           "}" "\n"
           ))
  (insert (concat
           "\n"
           "=item " setter                         "\n"
           "\n"
           "Setter for object attribute " setter    "\n"
           "\n"
           "=cut"                                  "\n"
           "\n"
           "sub " setter " {"                      "\n"
           "  my $self = shift;"                   "\n"
           "  my $" field " = shift;"              "\n"
           "  $self->{ " field " } = $" field ";"  "\n"
           "  return $" field ";"                  "\n"
           "}"                                     "\n"
           ))
  ))


;; The following isn't bad... but why not put this in your template.el
;; for OOP perl?
(defun perlnow-insert-new ()
  "Insert a basic framework for a 'new' method"
  (interactive)

  (let ((name "new"))
    (insert (concat
             "\n"
             "=item " name "\n"
             "\n"
             "Instantiates a new object of this class." "\n"
             "\n"
             "=cut"                     "\n"
             "\n"
             "sub " name " {"           "\n"
             "  my $class = shift;"     "\n"
             "  my $self = { "          "\n"
             "  #  fill in: "           "\n"
             "  #              name => 'value', " "\n"
             "  # "                     "\n"
             "  };"                     "\n"
             "  bless $self, $class;"
             "\n"
             "  lock_keys(%{ $self });"
             "\n"
             "  return $self;"
             "\n"
             "}" "\n"
             ))
  (search-backward "=item")
  (forward-line 3)
  ))


;;; Wrappers around external commands
; another proof-of-concept
; TODO:
; o  uses grep-find internally, but really needs it's own history. ((Why?))
;    Look at how grep-find is written, and use those constructs.
; o  Presumes --nocolor and --nogroup aren't entered in ack-search
;    forcibly remove if they're there?  Does that matter? Check.
; o  Really this isn't just for running perl code.
;    Maybe, write a package of external command wrappers?

(defun perlnow-ack (ack-search)
  "Does searches with the utility ack, ala grep-find."
  (interactive "sDo ack search: ")
  (let (
        (ack-command (format "ack --nocolor --nogroup %s" ack-search))
        )
       (let ((null-device nil))		; see grep
         (grep ack-command))))

(provide 'perlnow)

;;; perlnow.el ends here

