;; perlnow.el                Wed January   14, 2004
;;;                     Rev: Tue September 22, 2009
;;;                          Tue March     28, 2017

;;; Emacs extensions to speed development of perl code.

;; Copyright 2004, 2007, 2009, 2017 Joseph Brenner
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.318 2009/12/04 09:16:23 doom Exp $
;; Keywords:
;; X-URL: http://obsidianrook.com/perlnow/

;; And thanks to:
;;  Quinn Weaver - bug fix for package names in inside-out OOP modules

;;; See LEGAL section below.

;;;==========================================================
;;; Commentary:
;;
;;  perlnow.el is intended to speed the development of perl code
;;  by automating some routine tasks.
;;
;;  For the documentation for this package, see the documentation for
;;  the dummy variable perlnow-documentation (and it's relatives), below.

;;; Code:
(provide 'perlnow)
(require 'cl-lib)
(require 'cperl-mode)
(require 'json)

(defconst perlnow-version "0.7"
  "The version number of the installed perlnow.el package.
Check <http://obsidianrook.com/perlnow/> for the latest.")

(defvar perlnow-trace nil
  "Set to t to enable subroutine trace messages.")

(defvar perlnow-debug nil
  "Set to t to enable some debug messages.")

(defvar perlnow-counter 0
  "Counter that's incremented for every perlnow-tron invocation.")

(defvar perlnow-documentation t
  "The introductory documentation to the perlnow.el package.
Also see the documentation for:
`perlnow-documentation-installation'
`perlnow-documentation-coding-standard'
`perlnow-documentation-terminology'
`perlnow-documentation-template-expansions'
`perlnow-documentation-tutorial'
`perlnow-documentation-test-file-strategies'

This package speeds the development of perl code by automating
some routine tasks.  When an idea strikes, it makes it easier to
jump into coding; and while you're coding it assists in checking,
running and debugging the code from within emacs.

The commands for initial code creation typically prompt for a
location and/or name, and open a file buffer using an appropriate
template.  Perlnow makes heavy use of the template.el package,
adding a number of templates designed for perl development.
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

\\[perlnow-cpan-module] -- to begin working on a new module for a
CPAN-style distribution, defaults to using milla.

\\[perlnow-run-check] - does a perl syntax check on the
current buffer, displaying error messages and warnings in
the standard emacs style, so that the next-error command,
\(usually bound to control-x back-apostrophe\)
will skip you to the location of the problem.

\\[perlnow-run] - like the above, except that it actually runs
the code, prompting the user for a run string it if it has not
been defined yet.

\\[perlnow-set-run-string] - Allows the user to manually
change the run-string used by perlnow-run.

\\[perlnow-perldb] - runs the perl debugger using the above run string.

A list of some important functions that require template.el:
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

The perlnow.el file must be placed somewhere that's in your `load-path'.

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
  \(setq `perlnow-dev-location'
      \(substitute-in-file-name \"$HOME/dev\"\)\)\n

  \(add-hook 'after-save-hook 'perlnow-add-current-pm-incspot-to-perl5lib\)

  \(perlnow-define-standard-keymappings\)

Alternately, if you'd like a different prefix than the
default \"C-c/\", you can supply it as an argument:

   \(perlnow-define-standard-keymappings \"\\C-c'\"\)

Or if you prefer, that entire function call can be replaced
with individual definitions like so, to make it easier
to modify them individually:

   \(global-set-key \"\\C-c/s\" 'perlnow-script\)
   \(global-set-key \"\\C-c/m\" 'perlnow-module\)
   \(global-set-key \"\\C-c/o\" 'perlnow-object-module\)
   \(global-set-key \"\\C-c/P\" 'perlnow-cpan-module\)

   \(global-set-key \"\\C-c/c\" 'perlnow-run-check\)
   \(global-set-key \"\\C-c/r\" 'perlnow-run\)
   \(global-set-key \"\\C-c/d\" 'perlnow-perldb\)
   \(global-set-key \"\\C-c/R\" 'perlnow-set-run-string\)
   \(global-set-key \"\\C-c/t\" 'perlnow-edit-test-file\)
   \(global-set-key \"\\C-c/i\" 'perlnow-insert-sub\)
   \(global-set-key \"\\C-c/b\" 'perlnow-back-to-code\)
   \(global-set-key \"\\C-c/~\" 'perlnow-perlify-this-buffer-simple\)

The odd prefix \"control-c slash\" has been used because
only the C-c <punctuation> bindings are reserved for minor modes,
\(while perlnow is not a minor-mode, it has some similarities:
many perlnow commands need to work from many different modes\).
The slash was choosen because on typical keyboards, it's
unshifted and on the opposite side from the \"c\".

You might prefer other assignments, such as using function keys
for frequently used commands.  Some examples:

  \(global-set-key [f4] 'perlnow-script\)

  \(add-hook 'cperl-mode-hook
          '\(lambda \(\)
             \(define-key cperl-mode-map [f6] 'perlnow-perl-check\) \)\)

If you're looking for a good prefix for \"perl\" commands, remember
that \"M-p\" is used in many contexts for \"history\" navigation.
And be aware that \"C-x p\" is used by the p4.el package \(a
front-end to the proprietary perforce version control system\).

Platform coverage: perlnow.el is known to work with GNU emacs
versions 21 through 25, and was developed under linux
\(aka GNU/Linux\). Reportedly, it does not work with xemacs.")

(defvar perlnow-documentation-terminology t
  "Definitions of some terms used here:

Note: perlnow uses the simplifying assumption that a perl
package is a perl module is a single *.pm file,
Actually, multiple packages can be contained in a single file,
but that's not done so often in practice.

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

MODULE NAME or PACKAGE NAME: perl's double colon separated name,
e.g. \"Modular::Stuff\".  Note: I suggest you avoid calling the
module a package to avoid confusion with a \"cpan package\"
\(i.e. a \"tarball\"\).

INC SPOT or MODULE ROOT or PACKAGE ROOT: a place where perl's
package space begins \(e.g. /usr/lib/perl\). Perl's @INC is a list
of different such \"inc spots\".  These are often named \"lib\".

STAGING AREA: the directory created by cpan builder tools
\(milla, module-starter, h2xs, etc \) for module development, a
hyphenized-form of the module name e.g. Modular-Stuff. Staging
areas contain a module root \(or \"inc spot\") called \"lib\".

DEV LOCATION: the place where you put your staging areas.

PERLISH PATH: this means a module path including double
colons \(alternate term: \"colon-ized\"\), e.g.
   /usr/lib/perl/Modular::Stuff

FILE SYSTEM PATH \(or FILESYS PATH\): as opposed to
\"perlish\".  This is the regular \'nix style slash
separated path.

FULL: usually means that the full path is included,
e.g. \"full file name\".

TEST SCRIPT: A file with extension *.t associated with the
current module/script\(?\), often something like ModuleName.t
or possibly Staging-Area.t.

TEST LOCATION: place where the test script\(s\) are for
a given module \(almost always named \"t\"\).

TEST POLICY: the information necessary to know where to
put a newly created test file and what to call it:
1 - the test path dot form, e.g. \"./t\";
2 - the definition of dot e.g. module-file-location vs. incspot;
3 - the naming style, e.g. hyphenized vs. base.")

(defvar perlnow-documentation-coding-standard t
  "With this, the perlnow 0.6 release, I'm beginning to define a \"perlnow
standard\" for perl development. The intent is that if you follow this
standard, using perlnow should go more smoothly. It's not my intent to cop-
out and insist that you have to follow this standard to use perlnow \(though
if I were saner, I might\). Consider this the \"alpha\" release.

 o  The only existing standard for perl projects is cpan-style, so you
    should get as close to it as possible, e.g.

       dev/Modular-Stuff/
                         lib/
                               Modular/Stuff.pm
                           t/
                               01-Modular_Stuff-do_stuff-basic_test.t
                         bin/
                               stuff_it

    Modules should be placed in one directory \(typically named \"lib\"), and
    tests should be gathered in a parallel \"t\" directory \(sub-directories
    of \"t\" can be used to organize them\).

    Command-line scripts \(if any\) should be kept in a \"bin\"
    or \"scripts\" directory.

    Integrating the work from multiple cpan-style projects is
    most simply done by just installing them on the system.
    (Though that's hardly ideal and you'll probably do something else.)

 o  I recommend initiating a cpan-style project with Miyagawa's \"milla\" (see
    App::Milla on CPAN), which uses Module::Build::Tiny.  Use this until you
    have a reason to do something else.  See \\[perlnow-milla].

 o  Ideally, test files should target one sub in one module, and be named
    something like this \(for Modular::Stuff with sub do_something\):

      01-Modular-Stuff-do_something-basic_case.t

    Where the pattern is:

      <numeric prefix>-<Hyphenized Module Name>-<sub name>-<optional remark>.t

    The numeric prefix is an arbitrary (but ideally, unique) number whose
    sequence typically reflects the order of development of features \(or at
    least, the order of development of tests\).

    This is the sequence in which tests are *usually* run, but are not
    *required* to be run. Tests should be runnable independently, in
    random order.

 o  Scripts should typically be implemented with the main work done in
    modules: the main job of a script is to unpack and interpret arguments
    before passing them on to modules. Most testing should be implemented
    at the module level \(though perlnow has features to generate tests
    that shell out to run a script and check it's output\).

    You should not turn on taint \(-T\) in your hash-bang line unless you
    know you need it: perl complains if you try to invoke such a script in
    a different way without the -T.

    The only Getopt::* modules you need to understand are Getopt::Long and
    Getopt::Std. Seriously: just don't go there.

 o  I suggest initially beginning all module development using OOP, via the
    lightweight \"Moo\" package with \"MooX::Types::MooseLike::Base\". Then
    later if it turns out you need more flexible, dynamic OOP features,
    refactor to use \"Moose\". And alternately, if you're not doing much with
    object-state, consider refactoring to an Exporter-based non-OOP style.

 o  Use embedded-pod style, with sub documentation in an =item pod block
    immediately preceeding the code.

 o  You should be using cperl-mode, not the default perl-mode.

")


(defvar perlnow-documentation-tutorial t
  "First, see: `perlnow-documentation-installation'.

Depending on how you configure things, you should then have easy access
(perhaps as easy as a single keystroke of a function key) to some quick short-
cuts. Here's a run down on how you might use them for different purposes:

 `perlnow-documentation-tutorial-1-script-development'
 `perlnow-documentation-tutorial-2-module-development'
 `perlnow-documentation-tutorial-3-h2xs-module-development'
 `perlnow-documentation-tutorial-4-misc'
 `perlnow-documentation-test-file-strategies'")

(defvar perlnow-documentation-tutorial-1-script-development t
  "Got an idea for a script?  Hit \\[perlnow-script].

This will ask you for the name of the script you want to write, then kick you
into a file buffer with a recommended code template already filled in.

If you don't like the template, change it \(it should be in your ~/.templates
directory\).

Currently perlnow-script tends to want to put all of your new scripts in one
place, the `perlnow-script-location' that you've defined for it. You can, of
course, choose a different place to put a script at creation time: the default
is inserted into the minibuffer so that you can use it as a starting point to
edit into some new location. Similarly you can use your minibuffer history to
start with something you've used before (try Alt-p, the typical binding for
\\[previous-history-element]).

You also might want to change the default location on-the-fly: you can use
\\[set-variable] to change `perlnow-script-location'.

While you're working on the code, at any time, you can do a \"perlnow-run-check\"
(\\[perlnow-run-check]) to check for syntax errors and warnings.
This is a wrapper around the emacs compile-command facility running it with
\"perl -cw\".  Errors and warnings are listed in another buffer, and doing
a \"next-error\" rotates you through these, skipping you directly to the point
in the code where the problem was reported. By default, one runs \"next-error\"
via \"control-x back-apostrophe\"; and it looks like your current binding
is: \\[next-error]

Alternately, you might skip \\[perlnow-run-check] and go straight to
\\[perlnow-run], which will \(the first time through\) then ask you how you
want to run the script. The default command line is usually just
\"perl <scriptname>\"; but you can append whatever arguments and re-directs
you like. Once a run-string is defined for that file buffer it will stop
asking you this question, though you can change the run string later at any
time with \\[perlnow-set-run-string].

If you want to use the perl debugger, I suggest using \\[perlnow-perldb],
rather than \\[perldb]. The perlnow wrapper uses the `perlnow-run-string'
you've defined, which will be different for each perl file. If you use the
perldb command directly, you'll notice that the default is just however you
ran it last. If you're switching around, working on multiple files, that
default is going to be wrong a lot.

The next subject, developing perl modules:
  `perlnow-documentation-tutorial-2-module-development'")


(defvar perlnow-documentation-tutorial-2-module-development t
  "When you're interested in writing a module, the procedure
is similar to script development:
  `perlnow-documentation-tutorial-1-script-development'

There are multiple commands you could use to start on a new module.
The main ones are:

For proceedural modules:             \\[perlnow-module]
For object-oriented modules:         \\[perlnow-object-module]
For cpan-style modules:              \\[perlnow-cpan-module]

You could also run one of these directly:

 \\[perlnow-milla], \\[perlnow-h2xs], \\[perlnow-module-starter].

The first two are very similar, they just use different templates. Both ask
you for the name and location of the module you want to create in a single
prompt, asking for an answer in a hybrid form like:

  /home/hacker/perldev/lib/New::Module

Here the module location \(really, a \"module root\" location, or \"inc spot\",
see `perlnow-documentation-terminology') is entered in the usual file-system
form \(in this example, it is \"/home/hacker/perldev/lib/\"\) and the module name
is given using perl's double-colon separated package name notation \(in this
example, \"New::Module\"\).

The default for the module location is given by the variable
`perlnow-pm-location' which should be set in your .emacs as
indicated in `perlnow-documentation-installation'.  It can also
be modified on the fly with \\[set-variable].

Tab and space completion works while navigating the previously
existing part of the path \(including the part inside the package
name space\).  When you hit enter, it will create whatever
intervening directories it needs, after first prompting to make
sure it's okay (but setting the `perlnow-force' variable can
supress that question).

If you don't like this single-prompt method of entering this
information, you can use the older form of this command,
\\[perlnow-module-two-questions].

If you do a \\[perlnow-run] in a module buffer that doesn't yet
have a defined run string, perlnow will will try to find a way to
run it: it looks for a likely test file, before falling back on
asking you how to do it.

(( TODO fix the flow of remarks here, chop words if possible. ))

If you don't have a way to run it yet, you can first run
\\[perlnow-edit-test-file] to create a test file for the module,
or \\[perlnow-script] which will create an ordinary script using
the module.

Both of these commands will create files with a
\"use <module name>\" line filled in.  If the module is not in your
@INC search path, it will also add the necessary \"FindBin/use lib\"
magic to make sure that the script will be able to find the module.

If you skip back to the original module buffer, and do a \\[perlnow-run],
you'll notice that the script you just created has become the default
for the way the code in the module gets run.

At present, both the script and test creation commands capture
the name of the nearest \"sub\" and push it to the kill-ring,
where it can easily be retrieved via \\[yank].

Next, the cpan-style approach to module development:
  `perlnow-documentation-tutorial-3-cpan-style-module-development'")

(defvar perlnow-documentation-tutorial-3-cpan-style-module-development t
  "There's another completely different style of perl module development
from the one discussed in: `perlnow-documentation-tutorial-2-module-development';
which is oriented toward CPAN-style distributions.
These are usually set-up by running \"builder\" scripts.
There are perlnow front-ends for:
\\[perlnow-milla], \\[perlnow-module-starter] or \\[perlnow-h2xs].

This will ask you two questions: \(1\) where do you want to put
the build, aka the \"staging area\" \(default: the variable
`perlnow-dev-location'\) and \(2\) what do you want to call this
module.

Then you'll see two open windows, one showing the module file
buffer, the other showing the default test file for the module.

Note that \\[perlnow-module-starter] defaults to using the
Module::Build framework, while \\[perlnow-h2xs] defaults to using
the ExtUtils::MakeMaker approach.

The \\[perlnow-module-starter] default can be changed by
setting the `perlnow-module-starter-builder' variable.
Also, \\[perlnow-module-starter] respects the preference specified
by the `perlnow-module-style', which defaults to \"object\".

Next, the template naming convention:
 `perlnow-documentation-tutorial-4-template-naming-convention'")

(defvar perlnow-documentation-tutorial-4-template-naming-convention t
  "There's a convention for naming templates so that perlnow
can find appropriate ones for different cases.  For example, if
you run \\[perlnow-module-starter] using the default settings,
it will use two templates named:

  TEMPLATE.perlnow-modstar-module_build-object-pm.tpl
  TEMPLATE.perlnow-modstar-module_build-object-pm-t.tpl

Here \"modstar\" corresponds to \"module_starter\",
\"module_build\" means it's for a Module::Build cpan-style
distribution, and \"object\" means it's for OOP development.

Next, everyone's favorite subject, \"Misc\":
 `perlnow-documentation-tutorial-5-misc'")


(defvar perlnow-documentation-tutorial-5-misc t
  "Misc topic 1 - perlify:

In addition to the old-style non-template.el fallback:
\\[perlnow-script-simple], there's another command that's
somewhat similar called: \\[perlnow-perlify-this-buffer-simple].
The \"perlify\" concept is to open an empty buffer for your code
in the usual way, and then afterwards insert a simple code
template and make the file executable.

Note that template.el plus a good perl template, plus that
post-emacs 21 trick for making scripts executable automatically
all gets you very close to having this functionality without any
help from perlnow.el... except for one little gotcha: many of us
do not use a file extension on our perl scripts, and without a
standard extension like '.pl', that makes it hard for template.el
to spot that you're creating one.  You would need to always do a
\\[template-new-file] instead of \\[find-file], and carefully
select the correct template as well as the file name.

Next:
 `perlnow-documentation-test-file-strategies'")


(defvar perlnow-documentation-test-file-strategies t
  "The \\[perlnow-run] and \\[set-perlnow-run-string] commands
try to find an appropriate test to exercise the code in the current
buffer.  This version (0.5+) of perlnow tries to simplify the
search procedure (and this behavior may *still* be subject to change in
the future):

First of all we can presume that test files all end with the
\".t\" extension, and they're organized in a directory named
\"t\".  It's fairly common (and standard for cpan-style projects)
for \"t\" to be located next to the main code directory (named
\"lib\" for cpan-style) where the module namespace begins.
I typically refer to this as the \"incspot\".

As presently implemented (circa version 0.5), `perlnow-edit-test'
will find a test file that matches it's naming convention \(see
`perlnow-documentation-coding-standard'\), or alternately it will
look for the most recently modified one, and if it finds nothing it
will create a new one according to the naming convention.

While there are several supported naming styles for test files,
perlnow currently defaults to a 'fullauto' style like so:
\"01-Modular-Stuff-sub_name.t\".  You can rename the files later
manually for clarification, this shouildn't confuse perlnow \(or
not too much\): it's recommended to stick to adding an additional
suffix to say something more specific about what's being tested.

There is now (as of version 0.5) a \\[perlnow-test-create]
command and a \\[perlnow-test-create-manually] both typically
bound to keystrokes \"a\" and \"A\", respectively (here, \"A\" is
for \"creAte\", because \"c\" already means \"check\").

The \\[perlnow-edit-test-file] command will also create this new
test file if it does not already exist.

The user defineable \"test policy\" dictates where these new test
files will go.  See \"test policy\" in `perlnow-documentation-terminology'.

Next:
 `perlnow-documentation-7-template-expansions'")



;;;==========================================================
;;  User Options, Variables
;;;==========================================================

(defun perlnow-fixdir (dir &optional root)
  "Fixes the DIR.
Conditions directory paths for portability and robustness.
Some examples:
 '~/tmp'             => '/home/doom/tmp/'
 '~/tmp/../bin/test' => '/home/doom/bin/test/'
Note: converts relative paths to absolute, using the current
default-directory setting, unless specified otherwise with the
ROOT option.  Note side-effect: converts the empty string into
the default-directory or the ROOT setting."
  (let ((return
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name dir root))))))
    return))

(defun perlnow-mkpath (dir)
  "Create directory DIR (and intervening levels) if it doesn't exist."
  (unless (file-directory-p dir)
     (make-directory dir t)))

(defcustom perlnow-script-location
  (cond ( (file-directory-p  (perlnow-fixdir "$HOME/bin"))
          (perlnow-fixdir "$HOME/bin")
          )
        (t
         (perlnow-fixdir "$HOME")
         ))
  "This is the default location to stash new perl scripts.")

(defcustom perlnow-pm-location
  (cond ( (file-directory-p  (perlnow-fixdir "$HOME/lib"))
          (perlnow-fixdir "$HOME/lib")
          )
        (t
         (perlnow-fixdir "$HOME")
         ))
  "This is the default location to stash new perl modules.")

(defcustom perlnow-dev-location (file-name-as-directory perlnow-pm-location)
  "This is the default location to work on CPAN-style distributions.")

(defcustom perlnow-executable-setting ?\110
  "The user-group-all permissions used to make a script executable.")

;; TODO why not another defcustom?  (( let's see ))
;; (defvar perlnow-template-location (perlnow-fixdir "$HOME/.templates")
(defcustom perlnow-template-location (perlnow-fixdir "$HOME/.templates")
  "Standard location for template.el templates.")
(put 'perlnow-template-location 'risky-local-variable t)

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

No warranty is provided with this code.

See http://dev.perl.org/licenses/ for more information."
  "Software license message available to templates as LICENSE.
The default value is the traditional boilerplate for open source perl code.")

(defcustom perlnow-bugs-message
  "Please report any bugs or feature requests to C<bug-emacs-run at rt.cpan.org>, or
through the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Emacs-Run>.
I will be notified, and then you'll automatically be notified of progress
on your bug as I make changes."
  "Bug reporting instructions which can be inserted in templates using
the tag \(>>>BUGS<<<\)." )

(defcustom perlnow-sub-doc-pod "=item"
  "Pod tag used to introduce a block of sub documentation.
Defaults to '=item', and should probably stay that way, but I've known
people who wanted to use '=head3'.")

(defcustom perlnow-quiet nil
  "Makes file creation operations just work.
Silently creates directories if needed, and overwrites if it
exists already.  Copies of old files should be preserved with an
'.OLD' suffix ((TODO make this consistent)).")
;; See perlnow-create-with-template, which is used by all the
;; perlnow create commands.
(put 'perlnow-quiet  'risky-local-variable t)

(defvar perlnow-perl-script-name nil
  "Used internally to pass the script name to some templates.
Defines the PERL_SCRIPT_NAME expansion.")

(defvar perlnow-perl-package-name nil
  "Used internally to pass the module name to the new module template.
Defines the PERL_MODULE_NAME expansion.")

(defvar perlnow-perl-sub-name nil
  "Used internally to pass the script name to some templates.
Defines the PERL_SUB_NAME expansion.")

(defvar perlnow-package-name-history nil
  "The minibuffer history for perl modules.")

(defvar perlnow-run-string-history nil
  "The minibuffer history for perl run string settings.")

;; TODO should test file "guesses" make entries into this history?
(defvar perlnow-test-file-history nil
  "The minibuffer history for perl test files.")

(defvar perlnow-ack-history nil
  "The minibuffer history for the \\[perlnow-ack] command.")

(defconst perlnow-slash (convert-standard-filename "/")
  "A more portable form of the file system name separator.")
;; I'm using this instead of "/" to improve portability to for windows.
;; But even if this helps, there are still other places
;; dependencies have crept in, e.g. patterns that use [^/].
;; (And what about a root of "/" vs "C:\" ?)


;; Defining additional "expansions" for use in template.el templates.
;;
(defvar perlnow-documentation-7-template-expansions t

  "The perlnow template.el templates use some custom expansions
defined in perlnow.el.  A template.el \"expansion\" is a place
holder in the template that gets replaced by something else when
the template is used.  For example, \(>>>DATE<<<\) will become
the current date.

The perlnow custom expansions:

\(>>>EMAIL_DOT_EMACS<<<\)
This inserts the users email address as determined from
their .emacs setting of the variable `user-mail-address'.

\(>>>LICENSE<<<\)
Inserts the code licensing message from the variable
`perlnow-license-message'.

\(>>>BUGS<<<\)
Inserts the bug reporting procedure from the variable
`perlnow-bugs-message'.

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

;; Now the actual definitions:

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

;;       '("PERL_SUB_NAME" (insert perlnow-perl-sub-name) )
(setq template-expansion-alist
      (cons
       '("PERL_SUB_NAME" (insert (or perlnow-perl-sub-name "")) )
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

(setq template-expansion-alist
      (cons
       '("BUGS" (insert perlnow-bugs-message))
       template-expansion-alist))


(defvar perlnow-minimum-perl-version "5.10.0"
  "The minimum perl version you are interested in supporting.
This is used to define the template expansion for
\(>>>MINIMUM_PERL_VERSION<<<\).  For versions of perl later than
5.006, version numbers looking like 5.7.0 or 5.8.2 were often
used.  My impression -- the subject is actually insanely
complicated -- is that you can use the new form now without any
trouble, though you might want to play it safe with a version
such as \"5.008002\" rather than \"5.8.2\".")

;; Defining feature MINIMUM_PERL_VERSION to insert the above as an
;; an "expansion" in a template.el template: (>>>MINIMUM_PERL_VERSION<<<);
(setq template-expansion-alist
      (cons
       '("MINIMUM_PERL_VERSION" (insert perlnow-minimum-perl-version))
       template-expansion-alist))

;;; The following variables are always buffer-local (the
;;; reasoning behind the admonition against this in the emacs
;;; lisp reference doesn't seem to apply here).

;;; It could be there are too many of these: (I have separate module and
;;; script runstrings variables, and the one actual run-string, keeping
;;; straight how they're supposed to work is difficult: TODO).
;;; However, I refuse to expose a more complex data-structure
;;; (e.g. an alist) to the user as a customization mechanism.

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


(defvar perlnow-select-test-file-buffer-name "*select test file*"
  "Name of buffer to display lists of test files.")

(defvar perlnow-bashrc-include-file
  (substitute-in-file-name "$HOME/.bashrc_perl5lib_add")
  "Full name of a .bashrc include file to make additions to PERL5LIB permanent.")
;; (setq perlnow-bashrc-include-file (substitute-in-file-name "$HOME/.bashrc_perl5lib_add"))

;;;==========================================================
;;; test file search and creation settings

;; TEST POLICY: the information necessary to know where to
;; put a newly created test file and what to call it:
;; 1 - the test path dot form, e.g. \"./t\";
;; 2 - the definition of dot e.g. module pm-location vs. incspot;
;; 3 - the naming style, e.g. hyphenized vs. base.")

;; In the following there's one general-purpose default policy,
;; and multiple sets of default policies for particular purposes.

;; Implementation note: I could certainly put all of these
;; perlnow-test-policy-* settings into a more complex data
;; structure, but would that make them any easier for a user to
;; understand and modify?  If not, then what would be the point?

;; (defcustom perlnow-test-policy-test-location  "./t" ;; Old style, pre 0.5
(defcustom perlnow-test-policy-test-location  "../t"
  "Test location for newly created test files.
May be specified using a \"dot form\", relative to
`perlnow-test-policy-dot-definition'.  E.g. \"./t\",
\"../t\", \"~/my_test_files\" etc.
Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")
;; (setq perlnow-test-policy-test-location  "../t")

;; (defcustom perlnow-test-policy-dot-definition  "fileloc"  ;; old, deprecated
(defcustom perlnow-test-policy-dot-definition  "incspot"
  "The meaning of the \".\" in `perlnow-test-policy-test-location'.
Currently a string with two allowed values: \"fileloc\" or \"incspot\".
If \"fileloc\", we want to specify a location relative to the file's
file system path.  If \"incspot\" we want to specify a location
relative to the root of the module name space. E.g. for \"Modular::Stuff\"
the fileloc is the directory \"Modular\", and the incspot is
the location of the directory \"Modular\".
Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")
;; (setq perlnow-test-policy-dot-definition  "incspot")

;;(defcustom perlnow-test-policy-naming-style "numeric"
(defcustom perlnow-test-policy-naming-style "fullauto"
  "Naming style to be used in creating a new test file for a module.
To illustrate by example the supported naming styles for a module
named Modular::Stuff:

 o   fullauto:    01-Modular-Stuff-sub_name.t
 o   numeric:     01-Modular-Stuff.t
 o   hyphenized:  Modular-Stuff.t
 o   basename:    Stuff.t

Used by \\[perlnow-edit-test-file].  See:
`perlnow-documentation-test-file-strategies'.")
;; (setq perlnow-test-policy-naming-style "fullauto") ;; DEBUG

(defcustom perlnow-test-policy-test-location-cpan
  perlnow-test-policy-test-location
  "Like `perlnow-test-policy-test-location-cpan' but for cpan modules.")
(defcustom perlnow-test-policy-dot-definition-cpan
  perlnow-test-policy-dot-definition
  "Like `perlnow-test-policy-dot-definition', but for cpan modules.")
(defcustom perlnow-test-policy-naming-style-cpan
  perlnow-test-policy-naming-style
  "Like `perlnow-test-policy-naming-style', but for cpan modules.")

(defcustom perlnow-test-policy-test-location-module
  perlnow-test-policy-test-location
  "Like `perlnow-test-policy-test-location', but for non-cpan modules.")
(defcustom perlnow-test-policy-dot-definition-module
  perlnow-test-policy-dot-definition
  "Like `perlnow-test-policy-dot-definition', but for non-cpan modules.")
(defcustom perlnow-test-policy-naming-style-module
  perlnow-test-policy-naming-style
  "Like `perlnow-test-policy-naming-style', but for non-cpan modules.")

(defcustom perlnow-test-policy-test-location-script
  perlnow-test-policy-test-location
  "Like `perlnow-test-policy-test-location', but for scripts.")
(defcustom perlnow-test-policy-dot-definition-script
  "fileloc"
  "Like `perlnow-test-policy-dot-definition', but for scripts.")
(defcustom perlnow-test-policy-naming-style-script
  "basename"
  "Like `perlnow-test-policy-naming-style', but for scripts.")

;; DEBUG SECTION TODO when settings here are golden,
;; port up to the above defcustoms, and drop these setqs
(setq perlnow-test-policy-test-location         "../t")
(setq perlnow-test-policy-dot-definition        "incspot")
(setq perlnow-test-policy-naming-style          "fullauto")

(setq perlnow-test-policy-test-location-cpan    "../t")
(setq perlnow-test-policy-dot-definition-cpan   "incspot")
(setq perlnow-test-policy-naming-style-cpan     "fullauto") ;; was 'numeric', re-test

(setq perlnow-test-policy-test-location-module   "../t")
(setq perlnow-test-policy-dot-definition-module  "incspot")
(setq perlnow-test-policy-naming-style-module    "fullauto")

(setq perlnow-test-policy-test-location-script   "../t")
(setq perlnow-test-policy-dot-definition-script  "fileloc")   ;; override
;; TODO EXPERIMENTAL fullauto now best for scripts maybe?
;; (setq perlnow-test-policy-naming-style-script    "basename")  ;; override
(setq perlnow-test-policy-naming-style-script    "fullauto")  ;; override

;;;==========================================================
;;; other policy settings

(defcustom perlnow-cpan-style "milla"
  "Instruct \\[perlnow-cpan-module] how to create a cpan-style project.
At present, perlnow.el supports: \"h2xs\", \"module-starter\", and \"milla\".
Defaults to milla.")

(defcustom perlnow-module-starter-builder "Module::Build"
  "Base module for a cpan-style distribution.
Should be one of: \"Module::Build\", \"Module::Install\", \"ExtUtils::MakeMaker\".")

(defcustom perlnow-module-style "object"
  "Type of module you usually prefer to create, e.g. \"object\", or \"exporter\".
Defaults to \"object\", which for better or worse is the current
standard.  Used by some routines, such as \\[perlnow-module-starter],
to choose a code template.  Note, there is no restriction on the
allowed values here, any arbitrary string can be used, provided
you have appropriate code templates that use it.")

(defvar perlnow-getter-prefix "get_"
  "Defines the naming convention for getters for object-oriented code.
Editorial: the default setting in perlnow.el is \"get_\", because that's
very common, but if you never use the (now deprecated) mutators, doesn't
it make more sense to use an empty string?")

(defvar perlnow-setter-prefix "set_"
  "Defines the naming convention for setters for object-oriented code.")

;;;==========================================================
;;; external programs, names and paths

(defcustom perlnow-perl-program "perl"
  "Set this to provide a hint about your preferred perl binary.
For example, make it \"/usr/local/bin/perl\" if you would rather
use that than the system's perl.  Defaults to just \"perl\"
\(and let's the shell path sort it out\).  Note: this is used only in
some cases, e.g. \\[perlnow-module-starter], where possible perlnow
uses whatever is specified in the hash-bang line.")

(defcustom perlnow-podchecker-program "podchecker"
  "Set this to provide a hint about your preferred podchecker binary.
Set this to the path and name of the program you would like to run
when \\[perlnow-run-check] is invoked with a prefix argument.")

(defcustom perlnow-perlcritic-program "perlcritic"
  "Set this to provide a hint about your preferred perlcritic binary.
Set this to the path and name of the program you would like to run
when \\[perlnow-run-check] is invoked with a prefix argument.")

(defcustom perlnow-simple-hash-bang-line "#!/usr/bin/perl -w"
  "A typical hash bang line for perl code.
Used only by the somewhat deprecated \"simple\" functions:
\\[perlnow-script-simple] \\[perlnow-perlify-this-buffer-simple]")

;;;--------
;;; plist stash from t/incspot associations

;; A global (not buffer-local for once) where perlnow can save the
;; associations between the "t" directories and the lib directories.
(defvar perlnow-incspot-from-t-plist ()
  "Pairs of \"t\" dirs and code dirs (e.g. \"lib\" directories, aka incspots).
It's expected that there will be a one-to-one relationship between
the location of a project's code directories and it's \"t\".
You can go from a *.pm file to a \"t\" via the test policy \"testloc\"
setting.  Once that's done, we'll save that relationship here,
so that later, given a *.t file, we'll be able to find the location
of the code it tests.")

(defvar perlnow-etc-location
  (perlnow-fixdir (concat "$HOME" perlnow-slash ".emacs.d" perlnow-slash "perlnow"))
  "Location in ~/emacs.d for miscellanious perlnow files.")
(perlnow-mkpath perlnow-etc-location)

(defvar perlnow-incspot-from-t-json-file (concat perlnow-etc-location "incspot_from_t.json")
  "A json file used to preserve the plist of t/incspot associations.")

;;;==========================================================
;;; internally used vars

(defvar perlnow-run-string nil
  "Tells \\[perlnow-run] how to run the code in a particular file buffer.
This is a buffer local variable which is set by the software,
and thus should not typically be set by the user directly.
See `perlnow-script-run-string' and `perlnow-module-run-string' instead.")
(put 'perlnow-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-run-string)

(defvar perlnow-run-string-harder nil
  "Tells \\[perlnow-run] how to run code in a buffer, if given C-u prefix.
This is a buffer local variable which is set by the software,
and thus should not typically be set by the user directly.
See `perlnow-script-run-string' and `perlnow-module-run-string' instead.
This variant will be used to remember a more through way of running some
code (e.g. a full barrage of tests, rather than just one test file).")
(put 'perlnow-run-string  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-run-string-harder)

(defvar perlnow-associated-code nil
  "A code file associated with the current buffer.
Typicially a module might be associated with it's test file,
and vice-versa.  Used by \\[perlnow-back-to-code].")
;; (put 'perlnow-associated-code  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-associated-code)

;; currently only deployed in the test select menu
(defvar perlnow-associated-buffer nil
  "A buffer associated with the current buffer.
Similar to perlnow-associated-code, but really contains a buffer object.
")
;; (put 'perlnow-associated-code  'risky-local-variable t)
(make-variable-buffer-local 'perlnow-associated-buffer)

(defvar perlnow-recent-pick nil
  "Internally used place to record a recent file selection.
This allows this code to offer the last selection again as
the default.")
(make-variable-buffer-local 'perlnow-recent-pick)

(defvar perlnow-recent-pick-global nil
  "Internally used place to record a recent file selection.
This allows this code to offer the last selection again as
the default.
This is a variant which is not buffer-local, to experiment
with both approaches.")
;; TODO right now I can't fathom why it would be buffer local.
;;      isn't it intended to be able to pass the recent-pick
;;      value around?  I think I'm currently setting it
;;      for the buffer that was picked, which means you can't
;;      see the value unless you're already in that buffer.
;;      WTF?

(defvar perlnow-last-buffer-file-name ""
  "A global used to pass the last buffer-file-name to a hook.
See \\[perlnow-associate-last-with-current].")


;;;==========================================================
;;; set-up functions

;; This version is DRYer, via some hackery with eval-read in mode hooks.
(defun perlnow-define-standard-keymappings ( &optional prefix )
  "Quickly define some recommended keymappings for perlnow
functions.  By default, perlnow.el makes no changes to the user's
keymap, but this function is provided to make it easy to adopt a
standard set of keymappings.  Note: these all use the \"C-c/\"
prefix by default, unless a different PREFIX is supplied.  A few
mappings are also included for useful functions that are defined
outside of perlnow:
\\[cperl-perldoc-at-point], \\[comment-region] and \\[narrow-to-defun]."
  (interactive)
  (unless prefix (setq prefix "\C-c/"))
  ;; These need to be defined widely in all (or most) modes
  ;; because they're for jumping into writing perl code.
  (global-set-key (format "%ss" prefix) 'perlnow-script)
  (global-set-key (format "%sm" prefix) 'perlnow-module)
  (global-set-key (format "%so" prefix) 'perlnow-object-module)
  ;; (global-set-key (format "%sh" prefix) 'perlnow-h2xs)
  ;; (global-set-key (format "%sO" prefix) 'perlnow-module-starter)
  (global-set-key (format "%sP" prefix) 'perlnow-cpan-module)

  ;; These bindings can be specific to the user's favorite perl mode.
  (let ( (define-perl-bindings-string
           (replace-regexp-in-string
            "%s" prefix
            "'(lambda ()
               (local-set-key \"%sc\" 'perlnow-run-check)
               (local-set-key \"%sd\" 'perlnow-perldb)
               (local-set-key \"%sr\" 'perlnow-run)
               (local-set-key \"%sR\" 'perlnow-set-run-string)

               (local-set-key \"%st\" 'perlnow-edit-test-file)
               (local-set-key \"%sa\" 'perlnow-test-create)
               (local-set-key \"%sA\" 'perlnow-test-create-manually)

               (local-set-key \"%sb\" 'perlnow-back-to-code)
               (local-set-key \"%s1\" 'cperl-perldoc-at-point)
               (local-set-key \"%s#\" 'comment-region)
               (local-set-key \"%sN\" 'narrow-to-defun)
               (local-set-key \"%si\" 'perlnow-insert-sub)

               (local-set-key \"%s*\" 'perlnow-display-inc-array)

;;                ;; in a perl buffer, use this wrapper around next-error
;;                (local-set-key \"C-x`\" 'perlnow-next-error)
               )"
            ))
         )
    (add-hook 'cperl-mode-hook (eval (read define-perl-bindings-string)))
    (add-hook 'perl-mode-hook  (eval (read define-perl-bindings-string)))
    ))

;;;==========================================================
;;; functions to run perl scripts        TODO BOOKMARK REAL CODE STARTS

(defun perlnow-run-check (arg)
  "Run a perl check on the current buffer.
This displays errors and warnings in another window, in the usual
emacs style: After running this, you can skip to the location of
the next problem with \\\[next-error].
This form of the command is something like \\\[cperl-check-syntax]
\(it has one less prompt, and does not require mode-compile.el\).
When run with a prefix argument \(i.e. after hitting C-u\), it
runs a more elaborate suite of checks, doing podchecker and
perlcritic in addition to a \"perl -cw\".
This skips using perlcritic or podchecker if it can't find them."
  (interactive "P")
  (if perlnow-trace (perlnow-message "Calling perlnow-run-check"))
  (let* ( (full-file (buffer-file-name))
          (location (file-name-directory full-file))
          (filename (file-name-nondirectory full-file))
          (default-directory location)
          (perl (perlnow-how-to-perl))

          ;; podchecker --help 2>1 | grep 'Usage:'
          (podchecker-probe
           (concat perlnow-podchecker-program " --help 2>1 | grep 'Usage:'"))
          (podchecker-p (shell-command-to-string podchecker-probe))
          (podchecker (cond (podchecker-p perlnow-podchecker-program)
                            (t nil)))

          (podchecker perlnow-podchecker-program)
          ;; probe for perlcritic, set to nil if it's not installed.
          ;;   perlcritic --version  =>  1.102
          (perlcritic-probe (concat perlnow-perlcritic-program " --version 2>/dev/null"))
          (perlcritic-p (shell-command-to-string perlcritic-probe))
          (perlcritic (cond (perlcritic-p perlnow-perlcritic-program)
                            (t nil)))
          )
    (save-buffer)
    (unless perlcritic-p
      (message
       "perlcritic not installed: Install Perl::Critic from cpan,\ne.g. 'cpanm Perl::Critic'."))
    (unless podchecker-p
      (message
       "podchecker not found. You can install it from cpan,\n e.g. 'cpanm Pod::Checker'"))
    (cond ( (not arg) ; no prefix
            (setq compile-command
                  (format "%s -Mstrict -cw \'%s\'" perl filename))
            )
          (t ; C-u prefix
           (setq compile-command
                 (concat
                  (format "%s -Mstrict -cw \'%s\'" perl filename)
                  (cond ( podchecker
                          (concat
                           "; "
                           (format "%s \'%s\'" podchecker filename)
                           )))
                  (cond ( perlcritic
                          (concat
                           "; "
                           (format "%s --nocolor --verbose 1 \'%s\'" perlcritic filename)
                           )))
                  ))
           ))
    (message "compile-command: %s" compile-command)
    (compile compile-command);; this just returns buffer name "*compilation*"
    ))

(defun perlnow-run (run-string &optional harder-setting)
  "Run the perl code in this file buffer.
This uses a RUN-STRING which may be determined from
from the numeric HARDER-SETTING \(C-u means \"do it harder\"\)
and some buffer-local variables which remember previous settings
`perlnow-run-string' and `perlnow-run-string-harder'.
If those variables are nil, it will run a routine to try
to guess a good RUN-STRING  -- TODO fill that in --
The user will be asked to confirm a run-string the first
time it is used \(this happens via \\[perlnow-set-run-string]\)."
  (interactive
   (let* (
          (harder-setting (car current-prefix-arg))
          (existing-run-string
           (cond ( harder-setting
                   perlnow-run-string-harder
                   )
                 (t
                  perlnow-run-string
                  )
                 ))
          (run-string
           (or
            (if (not (string= existing-run-string ""))
                existing-run-string)
            (perlnow-set-run-string harder-setting)
            ))
          )
     (list run-string harder-setting)
     ))
  (if perlnow-trace (perlnow-message "Calling perlnow-run"))
  ;; saving the result (*possibly* redundant, but can't hurt)
  (perlnow-sync-save-run-string run-string harder-setting)
  (if run-string
      (compile run-string)
    ))

(defun perlnow-set-run-string (&optional harder-setting)
  "Prompt the user for a new run string for the current buffer.
This sets the global variable `perlnow-run-string' that
\\[perlnow-run] will use to run the code in the current buffer.
To manually change the run string, the user needs to run this
function directly, but it is also called indirectly by commands
such as \\[perlnow-run] if the run string is not yet defined.\n
When run with a \"C-u\" prefix \(or non-interactively, with
HARDER-SETTING passed in\) tries to guess a more thorough way to
run the code \(e.g. a test suite instead of a single test file\),
and works with the `perlnow-run-string-harder' variable."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-set-run-string"))
  (let* ( (harder-setting (or harder-setting (car current-prefix-arg)))
          (module-p (perlnow-module-code-p))
          (run-string)
          )
    ;; set-up default value for run-string
    (cond
     (module-p
      (setq run-string
            ;; funky "or"
            (cond (perlnow-module-run-string)
                  (t
                   (perlnow-guess-run-string harder-setting)))))
      (t  ;;  assume it's a script since it's not a module.
       (setq run-string
             ;; funky "or"
             (cond (perlnow-script-run-string)
                   (t
                    (perlnow-guess-run-string harder-setting)
                    )))))

    ;; ask user how to run this code (perlnow-run handles making this the default next time)
    (let ((prompt-for (cond (module-p "module") (t "script"))) )
        (setq run-string
              (read-from-minibuffer
               (format "Set the run string for this %s: " prompt-for)
               run-string
               nil
               nil
               (cons 'perlnow-run-string-history 2)
               )))
    (cond
     (module-p
      (setq perlnow-module-run-string run-string))
     (t  ;;  assume it's a script since it's not a module.
      (setq perlnow-script-run-string run-string)))
    ;; tell perlnow-run how to do it
    (perlnow-sync-save-run-string run-string harder-setting)))

(defun perlnow-perldb (run-string)
  "Run the perl debugger on the code in this file buffer.
This uses an interactively set RUN-STRING determined from
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
  (if perlnow-trace (perlnow-message "Calling perlnow-perldb"))
  (let* ((modified-run-string
          (replace-regexp-in-string "\\bperl " "perl -d " run-string))
         ;;;; TODO old dequote operation. Better: split-string-and-unquote
         (hacked-run-string
          (replace-regexp-in-string "'" "" modified-run-string))
         )
    (perldb hacked-run-string)))


(defun perlnow-ack (ack-search)
  "Does searches with the utility ack, ala grep-find.
Note: there's an ack.el package, if you'd like something fancier
than this."
  (interactive
   (let ( (history 'perlnow-ack-history)
          (keymap nil)  ;; use default keymap for grep command
          (initial nil) ;; no initial suggestion in minibuffer
          miniread
          inter-list
          )
     (setq miniread
           (read-from-minibuffer
            "Do code search with ack: "
            initial keymap nil history nil nil))
     (setq inter-list (list miniread))))
  (if perlnow-trace (perlnow-message "Calling perlnow-ack"))
  (let* ((ack-probe   (format "ack --version"))
         (ack-command (format "ack --nogroup %s" ack-search))
         )
    (cond
     ((shell-command-to-string ack-probe)
      (let ((null-device nil)) ;; see grep
        (grep ack-command))
      )
     (t
      (message "ack not installed.  Install App::Ack from cpan,\ne.g. 'cpanm App::Ack'.")
      )) ))

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
  (if perlnow-trace (perlnow-message "Calling perlnow-script"))
  ;; first check the script-name for obvious errors
  (cond ((string-match "::" script-name)
         (message
          "You really don't want to create a script with a '::' do you?"))
        ((string-match "\.pm$" script-name)
         (message
          "You really don't want to create a script ending in '.pm', right?"))
        (t ;; full speed ahead
         (require 'template)
         (let (package-name)
           ;; Note: perlnow-perl-package-name is used to pass name into template
           (cond
            (;; starting from module
             (setq package-name (perlnow-get-package-name-from-module-buffer))
             (let* ((pm-file (buffer-file-name)) ;;
                    (pm-location (file-name-directory pm-file))
                    (incspot (perlnow-get-incspot package-name pm-location)))
               (setq perlnow-perl-package-name package-name)
               (perlnow-do-script-from-module
                script-name package-name incspot)
               ))
            (;; starting from man page
             (setq package-name (perlnow-get-package-name-from-man))
             (setq perlnow-perl-package-name package-name)
             (perlnow-do-script-from-module script-name package-name))
            (t ;; no special starting place
             (perlnow-do-script script-name))))
         )))

;;;   TODO
;;;    When creating a module from man page, should check if it's in INC
;;;    Report problems, possibly by
;;;    Inserting comment in code file near use lib:
;;;         # Currently not found in @INC. Installed correctly?
;;;    Could use this to do the check:
;;;      (setq pm-file (perlnow-module-found-in-INC package-name))
;;;         ;; given colon-ized, returns first pm found, or nil if none
(defun perlnow-module (incspot package-name)
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
         ;; The keymap is key: transforms read-from-minibuffer.
         (keymap perlnow-read-minibuffer-map)
         (history 'perlnow-package-name-history)
         result filename retval
         )
     (setq result
           (read-from-minibuffer
            "New module to create \(e.g. /tmp/dev/New::Mod\): "
            initial keymap nil history nil nil))
     (setq result (replace-regexp-in-string "\.pm$" "" result))
        ;; remove accidentally typed ".pm"
     (setq filename
           (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm"))
     (while (file-exists-p filename)
       (setq result
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
              result keymap nil history nil nil))
       (setq filename
             (concat
              (replace-regexp-in-string "::" perlnow-slash result)
              ".pm")))
     (setq retval
           (perlnow-divide-hybrid-path-and-package-name
            result))
     retval))
  (if perlnow-trace (perlnow-message "Calling perlnow-module"))
  (require 'template)
  ;; global used to pass value into template
  (setq perlnow-perl-package-name package-name)
  (let ( (filename (perlnow-full-path-to-module incspot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))

(defun perlnow-object-module (incspot package-name)
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
         ;; keymap is key: transforms read-from-minibuffer.
         (keymap perlnow-read-minibuffer-map)
         (history 'perlnow-package-name-history)
         result filename return-list
         )
     (setq result
           (read-from-minibuffer
            "New OOP module to create \(e.g. /tmp/dev/New::Mod\): "
            initial keymap nil history nil nil))
     ;; remove accidentally typed ".pm"
     (setq result (replace-regexp-in-string "\.pm$" "" result))
     (setq filename
           (concat (replace-regexp-in-string "::" perlnow-slash result) ".pm"))
     (while (file-exists-p filename)
       (setq result
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
              result keymap nil history nil nil))
       ;; silently ignore accidentally typed ".pm"
       (setq result (replace-regexp-in-string "\.pm$" "" result))
       (setq filename
             (concat
              (replace-regexp-in-string "::" perlnow-slash result)
              ".pm")))
     (setq return-list
           (perlnow-divide-hybrid-path-and-package-name result))
     return-list)) ;; end interactive
  (if perlnow-trace (perlnow-message "Calling perlnow-object-module"))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let* ((filename (perlnow-full-path-to-module incspot package-name))
         (ret
          (perlnow-create-with-template filename perlnow-perl-object-module-template))
         )
    ret))

;;--------
;; the cpan-style "builder" suite: the perlnow-cpan-module wrapper

(defun perlnow-cpan-module (dev-location package-name)
  "Quickly jump into development of a new perl CPAN-style module.
This is a wrapper function that uses the `perlnow-cpan-style' setting
to determine how to work."
  (interactive
   (let ((default-directory perlnow-dev-location))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-message "Calling perlnow-cpan-module"))
  (let* ((func-str (concat "perlnow-" perlnow-cpan-style))
         (func-symbol (read (eval func-str)))
         )
    ;; Should be one of these (intentionally not checking this here):
    ;;   perlnow-h2xs
    ;;   perlnow-milla
    ;;   perlnow-module-starter
    (if perlnow-debug
        (message "perlnow-cpan-module cmd: %s %s %s " func-str dev-location package-name))
    (funcall func-symbol dev-location package-name)
    ))

(defun perlnow-h2xs (dev-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the DEV-LOCATION  \(the place where
h2xs will create the \"staging area\"\) and the PACKAGE-NAME \(in perl's
double-colon separated package name form\)."
  ;; Because default-directory is the default location for (interactive "D"),
  ;; I'm doing the interactive call in stages: this way can change
  ;; default-directory momentarily, then restore it. Uses the dynamic scoping
  ;; of elisp's "let" (which is more like perl's "local" than perl's "my").
  (interactive
   (let ((default-directory perlnow-dev-location))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-message "Calling perlnow-h2xs"))
  (setq dev-location (perlnow-fixdir dev-location))
  (unless (file-exists-p dev-location)
    (make-directory dev-location t))
  (let* ( display-buffer ;; buffer object
          (h2xs-module-file  "")
          (h2xs-test-file    "")
          (h2xs-staging-area "")
          (window-size 14)
          )
    (setq display-buffer (get-buffer-create "*perlnow-cpan-style*"))
    ;; Bring the *perlnow-h2xs* display window to the fore
    ;;   (bottom window of the frame)
    (perlnow-show-buffer-other-window display-buffer window-size t)
    (perlnow-blank-out-display-buffer display-buffer t)
    (let ((default-directory dev-location))
      ;; An h2xs run string looks like:  h2xs -AX -n Net::Acme -b 5.6.0
      (call-process "h2xs"
                    nil
                    display-buffer      ; must be buffer object?
                    nil
                    "-AX"
                    (concat "-n" package-name)
                    (concat "-b"
                            (perlnow-perlversion-old-to-new
                             perlnow-minimum-perl-version))))

    (setq h2xs-staging-area (perlnow-staging-area dev-location package-name))
    (perlnow-cpan-style-build h2xs-staging-area)
    (setq h2xs-module-file
          (perlnow-full-path-to-cpan-style-module dev-location package-name))
    (find-file h2xs-module-file)
    (search-forward "# Preloaded methods go here.")
    (forward-line 1)
    ;; Also  open the *.t file
    (setq h2xs-test-file (perlnow-full-path-to-dev-test-file h2xs-staging-area))
    (perlnow-open-file-other-window h2xs-test-file window-size)
    (perlnow-set-associated-code-pointers h2xs-module-file h2xs-test-file)
    ;; (funcall (perlnow-lookup-preferred-perl-mode))
    (goto-char (point-max))
    (other-window 1)
    ))

(defun perlnow-module-starter (cpan-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the CPAN-LOCATION  \(the place where
module-starter will create the \"staging area\"\) and the PACKAGE-NAME
\(in perl's double-colon separated package name form\)."
  ;; Because default-directory is the default location for (interactive "D"),
  ;; I'm doing the interactive call in stages: this way can change
  ;; default-directory momentarily, then restore it.
  ;; Uses the dynamic scoping of elisp's "let"
  (interactive
   (let ((default-directory perlnow-dev-location))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-message "Calling perlnow-module-starter"))
  (setq cpan-location (perlnow-fixdir cpan-location))

  (unless (file-exists-p cpan-location)
    (make-directory cpan-location t))
  (let* ( (cpan-template-tag "modstar")
          (display-buffer) ;; buffer object
          (module-file  "")
          (cpan-test-file    "")
          (cpan-staging-area "")
          (cpan-t-loc        "")
          (window-size 14)     ;; number of lines for the *.t file buffer
          (module-style perlnow-module-style)
;;           (builder-code
;;            (downcase
;;             (mapconcat 'identity (split-string perlnow-module-starter-builder  "::") "_")))
          )
    (setq display-buffer (get-buffer-create "*perlnow-cpan-style*"))

    ;;Bring the *perlnow-module-starter* display window to the fore (bottom window of the frame)
    (perlnow-show-buffer-other-window display-buffer window-size t)
    (perlnow-blank-out-display-buffer display-buffer t)

    (let* ((default-directory cpan-location)
           (modstar-cmd (perlnow-generate-module-starter-cmd  package-name cpan-location ))
           )
      (shell-command modstar-cmd display-buffer nil)

      (setq cpan-staging-area
            (file-name-as-directory
             (perlnow-staging-area cpan-location package-name)))
      (perlnow-cpan-style-build cpan-staging-area)
      (setq module-file
            (perlnow-full-path-to-cpan-style-module cpan-location package-name))

      (cond (perlnow-debug
             (message "perlnow-template-location: %s" perlnow-template-location)
             (message "cpan-template-tag: %s" cpan-template-tag)  ;; modstar
             (message "module-style: %s" module-style)  ))

      (setq cpan-t-loc (file-name-as-directory (concat cpan-staging-area "t")))
      (if perlnow-debug
          (message "cpan-t-loc: %s" cpan-t-loc))

      ;; create a module and test file using appropriate templates,
      ;; and swap the module file in place of the one module-starter creates
      (let* ( (pm-template
               (format
                "%sTEMPLATE.perlnow-%s-%s-pm.tpl"
                perlnow-template-location
                cpan-template-tag
                module-style
                ))
              (t-template
               (format
                "%sTEMPLATE.perlnow-%s-%s-pm-t.tpl"
                perlnow-template-location
                cpan-template-tag
                module-style
                ))
              )
        (cond ( perlnow-debug
                (message "pm-template: %s" pm-template)
                (message "t-template: %s" t-template) ))

        (require 'template)
        (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

        (delete-file module-file)
        (perlnow-create-with-template module-file pm-template)

        ;; clear the "t" directory, shuffling tests out of the way to "xt"
        (let ((dumping-grounds
               (concat cpan-t-loc (file-name-as-directory "..") "xt")) )
          (dolist (file (directory-files cpan-t-loc t "\\\.t$" t))
            (rename-file file dumping-grounds t)))

        ;; create and open the *.t file
        (setq cpan-test-file
              (perlnow-full-path-new-module-starter-test-file
                 cpan-staging-area package-name))
        (perlnow-open-file-other-window
          cpan-test-file window-size t-template t )
        (cond (perlnow-debug
               (message "module-file: %s"    module-file    )
               (message "cpan-test-file: %s" cpan-test-file )
               ))
        (perlnow-set-associated-code-pointers cpan-test-file module-file)
;;        (funcall (perlnow-lookup-preferred-perl-mode))
        ))))

(defun perlnow-generate-module-starter-cmd (module-name location)
  "Generate shell command string to run module-starter.
Creates a standard layout for development of a perl module named MODULE-NAME
in the directory LOCATION.
Get's the user's full name from the emacs function user-full-name
and the email address from the variable user-mail-address."
  (if perlnow-trace (perlnow-message "Calling perlnow-generate-module-starter-cmd"))
  (let* ( (author-name (user-full-name))
          (hyphenated (mapconcat 'identity (split-string module-name "::") "-"))
          (subdir (concat (file-name-as-directory location) hyphenated))
          (cmd
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
            perlnow-perl-program
            subdir
            )))
    (if perlnow-quiet
        (setq cmd (concat cmd " --force")))
    cmd))

(defun perlnow-milla (cpan-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the MODSTAR-LOCATION  \(the place where
module-starter will create the \"staging area\"\) and the PACKAGE-NAME
\(in perl's double-colon separated package name form\)."
  ;; Because default-directory is the default location for (interactive "D"),
  ;; I'm doing the interactive call in stages: this way can change
  ;; default-directory momentarily, then restore it.
  ;; Uses the dynamic scoping of elisp's "let"
  (interactive
   (let ((default-directory perlnow-dev-location))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-message "Calling perlnow-milla"))
  (setq cpan-location (perlnow-fixdir cpan-location))

  (unless (file-exists-p cpan-location)
    (make-directory cpan-location t))
  (let* ( (cpan-template-tag "milla")
          (display-buffer) ;; buffer object
          (module-file "")
          (cpan-test-file   "")
          (cpan-staging-area "")
          (cpan-t-loc "")
          (cpan-xt-loc "")
          (window-size 14)     ;; number of lines for the *.t file buffer
          (module-style perlnow-module-style)
;;           (builder-code
;;            (downcase (mapconcat 'identity (split-string perlnow-module-starter-builder  "::") "_")))
;;          (builder-code "module-build-tiny")
          pm-template t-template
          )
    (setq display-buffer (get-buffer-create "*perlnow-cpan-style*"))

    ;;Bring the *perlnow-module-starter* display window to the fore (bottom window of the frame)
    (perlnow-show-buffer-other-window display-buffer window-size t)
    (perlnow-blank-out-display-buffer display-buffer t)

    (let* ((default-directory cpan-location)
           (milla-cmd (concat "milla new " package-name ))
           )
      (if perlnow-debug (message "milla-cmd: %s" milla-cmd))
      (shell-command milla-cmd display-buffer nil)

      (setq cpan-staging-area
            (file-name-as-directory
             (perlnow-staging-area cpan-location package-name)))
      (perlnow-cpan-style-build cpan-staging-area)
      (setq module-file
            (perlnow-full-path-to-cpan-style-module cpan-location package-name))

      (cond (perlnow-debug
             (message "perlnow-template-location: %s" perlnow-template-location)
             (message "cpan-template-tag: %s" cpan-template-tag)  ;; milla
             (message "module-style: %s" module-style)  ))

      (setq cpan-t-loc (file-name-as-directory (concat cpan-staging-area "t")))
      (if perlnow-debug
          (message "cpan-t-loc: %s" cpan-t-loc))

      ;; let us be consistent with our module-starter handling,
      ;; hobgoblin of small minds though it is
      (setq cpan-xt-loc (file-name-as-directory (concat cpan-staging-area "xt")))
      (copy-directory cpan-t-loc cpan-xt-loc nil t t)
      (delete-directory cpan-t-loc t)
      (perlnow-ensure-directory-exists cpan-t-loc)

      ;; create a module and test file using appropriate templates, and swap
      ;; in the perlnow files in place of the ones generated by the builder
      (let* ( (pm-template
               (format
                "%sTEMPLATE.perlnow-%s-%s-pm.tpl"
                perlnow-template-location
                cpan-template-tag
                module-style
                ))
              (t-template
               (format
                "%sTEMPLATE.perlnow-%s-%s-pm-t.tpl"
                perlnow-template-location
                cpan-template-tag
                module-style
                ))
              )
        (cond ( perlnow-debug
                (message "pm-template: %s" pm-template)
                (message "t-template: %s" t-template) ))

        (require 'template)
        (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

        (delete-file module-file)
        (perlnow-create-with-template module-file pm-template)

        ;; create and open the *.t file
        (setq cpan-test-file
              (perlnow-full-path-new-module-starter-test-file
                 cpan-staging-area package-name))
        (perlnow-open-file-other-window
          cpan-test-file window-size t-template t)
        (cond (perlnow-debug
               (message "module-file: %s"    module-file    )
               (message "cpan-test-file: %s" cpan-test-file )
               ))
        (perlnow-set-associated-code-pointers cpan-test-file module-file)
;;        (funcall (perlnow-lookup-preferred-perl-mode))
        ))))



;;---------
;; edit-test and a few related functions
;; (aka the feature from hell, the Hard Part, the Serious Mess)
;;

(defun perlnow-edit-test-file ()
  "Find \(or create\) an appropriate testfile for the current perl code.
In interactive use, tries to identify a 't' directory related to
the current buffer, and if run without an argument, tries to
guess which test file is likely to be of interest.  If run with a
prefix argument (C-u) this opens a test select menu, allowing the
user to select a test file manually.

This function doesn't work reliably when called non-interactively,
instead you should most likely do this:

         (perlnow-open-test-file
          (perlnow-get-test-file-name))

Or if a specific testfile is known already:

         (perlnow-open-test-file testfile)

Or to simulate a calling prefix and open a test select menu:

   (perlnow-edit-test-file-harder 4)

"
  (interactive) ;; TODO how long has this been missing? -- Thu  April 06, 2017  19:30
  (if perlnow-trace (perlnow-message "* Calling perlnow-edit-test-file"))

  (let* ((harder-setting (car current-prefix-arg))
         testfile ;; no longer passed in as an argument (so this has been broken since...?)
         )
    (cond (
           (and harder-setting
                (> harder-setting 1))  ;; if so, perlnow-select-file has to handle open, etc
           (perlnow-edit-test-file-harder harder-setting))
          (t
           (cond ((not testfile) ;; no testfile given (an interactive run)
                  (setq testfile
                        (perlnow-get-test-file-name))
                  ))
           (perlnow-open-test-file testfile)
           ))))

;; Used by: perlnow-edit-test-file, perlnow-test-create-manually, perlnow-select-create-test
(defun perlnow-open-test-file (testfile)
  "The core of the interactive function \\[perlnow-edit-test-file].
When called, presumes that the current buffer displays code
to be associated with the given TESTFILE." ;; TODO expand docstring
  (if perlnow-trace (perlnow-message "* Calling perlnow-open-test-file"))
  (let* ((harder-setting  (car current-prefix-arg))
         (new-file-p      (not (file-exists-p testfile)))
         original-code
         package-name
         pm-file
         pm-location
         incspot
         )
    (perlnow-sub-name-to-var)
    (cond
     ;; TODO maybe should add a cpan-style handler here?
     (;; if module
      (setq package-name (perlnow-get-package-name-from-module-buffer))
       ;; define module incspot now, before opening test file buffer
         (setq pm-file      (buffer-file-name))
         (setq pm-location  (file-name-directory pm-file))
         (setq incspot     (perlnow-get-incspot package-name pm-location))
         (setq original-code   pm-file)
         ;; global to pass value to template
         (setq perlnow-perl-package-name package-name)
         (perlnow-open-file-other-window
          testfile 30 perlnow-perl-test-module-template)
         (funcall (perlnow-lookup-preferred-perl-mode))
         (if new-file-p
             (save-excursion
               (let* ((import-string
                       (perlnow-import-string-from package-name incspot))
                      (whitespace
                       (perlnow-jump-to-use package-name import-string) ))
                 (perlnow-endow-script-with-access-to incspot whitespace))))
         (save-buffer)
         )
     ;; if script
     ((perlnow-script-p)
      ;; global to pass value to template
      (setq perlnow-perl-script-name (buffer-file-name))
      (setq original-code   perlnow-perl-script-name)
      (perlnow-open-file-other-window
       testfile 30 perlnow-perl-test-script-template)
      (funcall (perlnow-lookup-preferred-perl-mode))
      (save-buffer))
     ;; if test select menu buffer (with associated pm)
     ((perlnow-test-select-menu-p)
      (setq package-name (perlnow-module-from-t-file testfile t))

      ;; (setq incspot (perlnow-stash-lookup (file-name-directory testfile)))
      (perlnow-incspot-from-t testfile
                               ;; policy-metadata ;; TODO
                               )
      (setq original-code (perlnow-full-path-to-module incspot package-name))

      ;; Note: the following is nearly identical to end of module-p  TODO refactor?
      ;; global to pass value to template
      (setq perlnow-perl-package-name package-name)
      (perlnow-open-file-other-window
       testfile 30 perlnow-perl-test-module-template)
      (funcall (perlnow-lookup-preferred-perl-mode))
      (if new-file-p
          (save-excursion
            (let* ((import-string
                    (perlnow-import-string-from package-name incspot))
                   (whitespace
                    (perlnow-jump-to-use package-name import-string) ))
              (perlnow-endow-script-with-access-to incspot whitespace)
              )))
      (save-buffer)
      )
     (t
      (let ((extension (perlnow-file-extension (buffer-file-name))))
        (cond ((string= extension "t")
               (message "Perlnow: You're already inside of a test file."))
              (t
               (message "Perlnow: Not a perl buffer.")
               )))))
    (cond ((and (file-exists-p testfile)
                original-code)
           (perlnow-set-associated-code-pointers testfile original-code)))
    (perlnow-sync-save-run-string
     (perlnow-generate-run-string testfile) harder-setting)
    ))


;; TODO as written, this presumes you're inside a module, doesn't it?
;;      if it's run inside a script, you get a badly named test:
;;           /home/doom/t/14--say_usage.t
;;      (if it has to be from a module, shouldn't it check that?)
;;
(defun perlnow-test-create (&optional testfile)
  "Create test file using automated guess."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-test-create"))
  (perlnow-sub-name-to-var)
  (let ((harder-setting (car current-prefix-arg)))
    (unless testfile ;; guess a default value for testfile
      (let* ( (md (perlnow-metadata))
              (testloc    (nth 3  md))
              (hyphenized (nth 4  md))
              )
        (setq testfile
              (perlnow-new-test-file-name testloc hyphenized))
        ))
    (cond (harder-setting  ;; not the main entry point for this, but what other behavior? TODO
           ;; minibuffer entry with testfile as default
           (perlnow-test-create-manually testfile)
           )
          (t
           (setq perlnow-recent-pick testfile)
           (setq perlnow-recent-pick-global testfile)
           (perlnow-open-test-file testfile)
           ))
    ))

(defun perlnow-test-create-manually (&optional testfile)
  "Create test file via minibuffer entry with TESTFILE as default."  ;; TODO expand
  (interactive)
  (unless testfile
    ;; guess a default value for testfile
    (let* ( (md (perlnow-metadata))
            (testloc    (nth 3  md))
            (hyphenized (nth 4  md))
            )
      (setq testfile
            (perlnow-new-test-file-name testloc hyphenized))
      ))
  (let* ((tf-input
          (read-from-minibuffer
           "Test file: "
           testfile
           nil
           nil
           (cons 'perlnow-test-file-history 2) ;; TODO double-check the 2
           ))
         )
    (setq perlnow-recent-pick tf-input)
    (setq perlnow-recent-pick-global tf-input)
    (perlnow-open-test-file tf-input)
    ))

;;-------
;; user-level navigation commands ((TODO rename? relocate?))

(defun perlnow-back-to-code ()
  "Return to the buffer that the current buffer is presently
associated with.  Perlnow commands typically record an
\"association\" between two buffers (using the buffer-local
variable `perlnow-associated-code').  For example, if you use a
perlnow command to open a test file from a module, afterwards
both buffers point at each other, so that \"back\" command can
easily be used to switch between them, even in cases where
\\[switch-to-buffer] would take you elsewhere.  If the buffer is
already displayed in another window of the current frame, this
will switch to that window."
  ;; Note perlnow-associated-code is automatically updated by
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-back-to-code"))
  (let* ( (other-buffer perlnow-associated-code)
          (search-other-frames nil)
          (existing-window (get-buffer-window other-buffer search-other-frames))
            ;; checks the selected frame only
          )
    (cond ( existing-window
            (select-window existing-window))
          (t
           (if other-buffer
               (find-file other-buffer)))
          )
    ))


;;========
;; guess run-string routines
;;   key routines for determining appropriate run-strings.

(defun perlnow-guess-run-string (&optional harder-setting)
  "Return a good guess for the perl run string for the current buffer.
If the HARDER-SETTING option is set, then it will try to run it \"harder\",
e.g. run all tests rather than just one."
  (if perlnow-trace (perlnow-message "Calling perlnow-guess-run-string"))
  ;; TODO document in more detail?  Or is that better done higher up?
  (let (
        (filename     (buffer-file-name))
        (associated   perlnow-associated-code)
        (staging-area (perlnow-find-cpan-style-staging-area))
        (testfile)
        (run-string) ;; the returned value
        )
    (cond (harder-setting
           ;; the cpan-style case
           (cond (staging-area
                  (setq run-string
                        (perlnow-cpan-style-test-run-string staging-area))
                  )
                 (t ; non-cpan-style code
                  (setq run-string
                        (perlnow-test-run-string-harder harder-setting))
                  )))
          (t ;; harder not set, so do a standard run
           (cond
            ( ;; if we are a test, don't look for another
             ;; (string-match "\\\.t$"  filename)
             (perlnow-test-p filename)

             (setq run-string (perlnow-generate-run-string filename)))

            ;; if there's an associated script/test already, just use that.
            ((and (perlnow-perlish-true-p associated)
                  (or
                    (perlnow-script-file-p  associated)
                    (perlnow-test-p associated)
                    ))
             (setq run-string (perlnow-generate-run-string associated))
             )

            ;; if we are a script... perhaps we should just run ourselves
            ((perlnow-script-p)
             (setq run-string (perlnow-generate-run-string filename)))
            (t ;; scrounge around for a *.t file to use
             ;; the cpan-style case
             (cond (staging-area
                    (setq run-string
                          (perlnow-generate-run-string-and-associate
                           (perlnow-latest-test-file
                            (perlnow-list-test-files
                             perlnow-test-policy-test-location-cpan  ;; "../t"
                             perlnow-test-policy-dot-definition-cpan ;; "incspot"
                             perlnow-test-policy-naming-style-cpan   ;; "fullauto"
                             t  ;; return fullpath
                             t  ;; recurse
                             ))))
                    )
                   (t ; non-cpan-style
                    (setq testfile (perlnow-get-test-file-name))
                    (cond ( (not (file-exists-p testfile))
                            (perlnow-edit-test-file testfile) ;; creates a new test
                            ))
                    (setq run-string
                          (perlnow-generate-run-string-and-associate testfile))
                    ))
             ))) ;; end "standard run" cases
          )
    (if perlnow-trace
        (message "   Returning from 'guess'"))
    run-string))

(defun perlnow-cpan-style-test-run-string (staging-area)
  "Given STAGING-AREA, return appropriate run-string for tests."
  (if perlnow-trace (perlnow-message "Calling perlnow-cpan-style-test-run-string"))
  (let* (run-string)
    (setq staging-area (shell-quote-argument staging-area))
    (cond ((file-exists-p (concat staging-area "Build.PL"))
           (setq run-string (concat "cd " staging-area "; ./Build build && ./Build test"))
           )
          ((file-exists-p (concat staging-area "Makefile.PL"))
           (setq run-string (concat "cd " staging-area "; make test"))
           ))
    run-string
    ))

(defun perlnow-test-run-string-harder (harder-setting)
  "Generate a run-string for a through non-cpan-style test run.
Uses the HARDER-SETTING \(4 or 16\) to choose whether to do a
\"prove\" or \"prove -r\"."
  (if perlnow-trace (perlnow-message "Calling perlnow-test-run-string-harder"))
  (let* ((run-string)
         (t-dir (car (perlnow-find-t-directories)))
         )
    ;; if no t directory was found, try to create according to default policy.
    ;;    TODO test this behavior, new as of 2017
    (unless t-dir
      (setq t-dir (perlnow-testloc-from-policy
                     perlnow-test-policy-test-location-module
                     perlnow-test-policy-dot-definition-module
                     perlnow-test-policy-naming-style-module)))
    (setq run-string
          (cond ( (>= harder-setting 16) ;; even harder!
                  (concat "cd " t-dir "; prove --nocolor -r"))
                ( t
                  (concat "cd " t-dir "; prove --nocolor *.t"))
                ))
    run-string
    ))

;;;==========================================================
;;; window management

;;; Currently when I want to show a new window alongside the
;;; existing current window, I close all others and just
;;; display the two of them.

(defun perlnow-new-window-below (&optional numblines)
  "Like split-window-vertically, but handles window size issues without errors.
Optional NUMBLINES requests that many lines in the new, lower, window.
If there aren't at least twice as many lines available, the NUMBLINES request
is ignored, and we fall back on splitting into two equally sized windows."
  (if perlnow-trace (perlnow-message "Calling perlnow-new-window-below"))

  (let* ((lines-available (window-total-height))  ;; 9 when noninteractive (?)
         )
  (cond ( (and numblines (>= lines-available (* 2 numblines)))
          (split-window-vertically (* -1 numblines))
          )
        (t
          (split-window-vertically)
         ) )))

(defun perlnow-open-file-other-window (file &optional numblines template switchback)
  "Open FILE in another window, leaving the current buffer visible.
Options: NUMBLINES, the number of lines in the new
window (defaults to half of frame height); TEMPLATE a
template.el template to be used in creating a new file
buffer.  If SWITCHBACK is true, the cursor is left in the
original window, not the new one.
Note: as written the given FILE is expect to have the full path."
  (if perlnow-trace (perlnow-message "Calling perlnow-open-file-other-window"))
  (if perlnow-debug
      (message "\"open other\" w/ %s" (pp-to-string (list file numblines template switchback))))

  (unless perlnow-perl-sub-name
    (setq perlnow-perl-sub-name ""))

  ;; before you open, point at where you're going to be from here
  (setq perlnow-associated-code file)
  ;; and save name of what we're looking at
  (setq original-file-displayed (buffer-file-name))
      ;; n.g. if a display buffer without a file

  (delete-other-windows)
  (perlnow-new-window-below numblines)
  (other-window 1)
  (let ((location (file-name-directory file)))
    (cond ((file-exists-p file)
           (find-file file))
          (t ;; file does not exist yet
             ;; create directory if need be
           (unless (file-exists-p location)
             (make-directory location t))
           (if template
               (perlnow-create-with-template file template)
             (find-file file))
           ))
    (funcall (perlnow-lookup-preferred-perl-mode))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-show-buffer-other-window"))

  (unless numblines
    (setq numblines (/ (frame-height) 2) )) ; new window defaults to half of frame height
  (delete-other-windows)
  ;; (split-window-vertically numblines) ; Number of lines to display
  (perlnow-new-window-below numblines)
  (other-window 1)
  (switch-to-buffer buffer) ;; not set-buffer, so buffer is visible
  (if switchback
      (other-window 1))
  )


(defun perlnow-blank-out-display-buffer (buffer &optional switchback)
  "Clear out a temporary display BUFFER.
Erase the contents of a buffer, though only if it matches
the convention for temporary display buffers, i.e. it has
a name beginning with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument BUFFER can be a string or
a buffer object.  This can work on a read-only buffer."
  (if perlnow-trace (perlnow-message "Calling perlnow-blank-out-display-buffer"))
  (let ((original-buff (buffer-name))                   ;; TODO Not used, right?
        (original-default-directory default-directory)  ;; TODO What for?
        original-read-only-status)
    ;; Buffer argument may be string or buffer object
    (if (char-or-string-p buffer) ; stringp better ? would a char work?
        (setq buffer (get-buffer-create buffer)))
    (if (not (string= "*" (substring (buffer-name buffer) 0 1)))
        (error "Will not blank out a buffer that does not begin with \"*\""))
    ;; clear buffer if it exists, create it otherwise
    (if (buffer-live-p buffer)
        (progn
          (set-buffer buffer)
          (setq original-read-only-status buffer-read-only)
          (setq buffer-read-only nil) ;; make sure buffer is writeable
          (mark-whole-buffer) ;; supposedly for interactive use only?
          (delete-region (mark) (point))
          (setq buffer-read-only original-read-only-status) ;; make it read-only if we found it that way
          )
      (get-buffer-create buffer))
    (if switchback
        (set-buffer buffer))
    (setq default-directory original-default-directory)))


;;=======
;; internal, lower-level routines used by the external "entry point" functions

(defun perlnow-do-script (filename)
  "Quickly jump into development of a new perl script.
Prompts the user for the FILENAME.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script]."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (if perlnow-trace (perlnow-message "Calling perlnow-do-script"))
  ;; (message "pong A") ;; DEBUG
  (require 'template)
  (perlnow-create-with-template filename perlnow-perl-script-template)
  (perlnow-change-mode-to-executable))

(defun perlnow-do-script-from-module (script package &optional incspot)
  "Does the work of creating a script from a module-buffer.
Takes arguments SCRIPT, PACKAGE, and INC-SPOT,
which are all explained in `perlnow-documentation-terminology'.
If INC-SPOT is nil, it skips adding the FindBin/use lib lines.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script] (or possibly the older
\\[perlnow-script-using-this-module])."
  (if perlnow-trace (perlnow-message "Calling perlnow-do-script-from-module"))
  ;; Presumption: if incspot is nil, then we got here from a man page buffer,
  ;; and we can assume the module is installed (or the man page most
  ;; likely wouldn't be there).
  (let* ((initial (current-buffer))
         (man-page-p (eq incspot nil))
         (created))
;;     (unless man-page-p
;;       (perlnow-sub-name-to-kill-ring))
    (unless man-page-p
      (perlnow-sub-name-to-var))

    ;; module is displayed, now want to open script, show in paralel
    (perlnow-open-file-other-window
       script
       nil
       perlnow-perl-script-template)
    (setq created (current-buffer))

    ;; Make the script we've created the default run-string for this module.
    (set-buffer initial)
    (setq perlnow-run-string
          (cond (man-page-p
                 (perlnow-generate-run-string script))
                (t
                 (perlnow-generate-run-string-and-associate script))
                ))
;;    (set-buffer created)
    (switch-to-buffer created)
    ;; forget about a "use" line for things that don't look like perl modules.
    (let ( (case-fold-search nil)
           (import-string "" ) )
      (if (string-match "^[A-Z]" package) ;; Module::Names are capitalized
          (progn
            (unless (eq incspot nil)
              (perlnow-endow-script-with-access-to incspot)
              (setq import-string
                    (perlnow-import-string-from package incspot))
              )
            ;; insert the "use Modular::Stuff;" line
            (insert (format "use %s%s;" package import-string))
            (insert "\n")
            )))
    (perlnow-change-mode-to-executable)
    script))

(defun perlnow-endow-script-with-access-to (location &optional whitespace)
  "Insert appropriate \"use lib\" line so script will see given LOCATION."
  (if perlnow-trace (perlnow-message "Calling perlnow-endow-script-with-access-to"))
  (unless (perlnow-incspot-in-INC-p location)
    (let* ((script-name (buffer-file-name))
           (relative-path
            (file-relative-name location (file-name-directory script-name))))
      (unless whitespace (setq whitespace "")) ;; Default to empty string
      (insert (format "%suse FindBin qw\($Bin\);\n" whitespace))
      (insert (format "%suse lib \(\"$Bin/" whitespace))
      (insert relative-path)
      (insert "\");\n"))))

;; Used by:
;;   perlnow-open-test-file
;;   perlnow-do-script-from-module

;; Old:
;; This will just be ':all' \(if the module is Exporter\)
;; based, or the empty string if not.

(defun perlnow-import-string-from (package-name &optional incspot-opt)
  "Get a workable import string from the module PACKAGE-NAME.
If the module is not exporter-based, this is just the empty string,
if it is, it will be the contents of the EXPORT and EXPORT_OK arrays.
If the module is installed, this can work just with the
PACKAGE-NAME, otherwise, the optional INC-SPOT-OPT is needed to point
at it."
  (if perlnow-trace (perlnow-message "Calling perlnow-import-string-from"))
  (if perlnow-debug
      (message "\"import-string\" w/ %s %s" package-name (pp-to-string incspot)))
  (let* (
         (incspot
          (cond (incspot-opt)
                (t
                 (perlnow-get-incspot
                  package-name
                  (file-name-directory
                   (perlnow-module-found-in-INC package-name))))))
         (module-file (perlnow-full-path-to-module incspot package-name))
         (original-buffer (current-buffer))
         (import-string "")
         is  ;; dummy var, more readable (?)
         export-string
         )
    (save-excursion
      (find-file module-file)
      (setq import-string
            (cond ((perlnow-exporter-code-p)
                   (setq export-string
                         (mapconcat 'identity
                                    (perlnow-export-list-for package-name incspot)
                                    " "))
                   (setq is
                         (concat
                          " qw("
                          export-string
                          ") ")))
                  (t
                   ""
                   ))))
    (switch-to-buffer original-buffer) ;; save-excursion?  Hello?
    import-string))

;; ========
;; efficient set subtraction (using a hash table)
;; (used by perlnow-revise-export-list)

(defun perlnow-string= (a b)
  "Essentially just another string=.
You may call it redundant, but it makes me feel beter."
  (if perlnow-trace (perlnow-message "Calling perlnow-string="))
  (compare-strings a nil nil b nil nil nil))

(defun perlnow-string-hash (key)
  "Tries (and evidentally, fails) to strip text properties."
  (if perlnow-trace (perlnow-message "Calling perlnow-string-hash"))
  (let (( limit (- (length key) 1) ))
    (sxhash
     (progn
       (set-text-properties 0 limit nil key)
       key))))

(define-hash-table-test 'perlnow-test
 'perlnow-string= 'perlnow-string-hash)

(defun perlnow-subtract-lists (list1 list2)
  "Subtracts LIST2 from LIST1.
Returns a list containing values of LIST1 that are not
found in LIST2."
  (if perlnow-trace (perlnow-message "Calling perlnow-subtract-lists"))
  (let* (
         ;; (seen (make-hash-table :test 'equal))
         ;; (seen (make-hash-table))
         (seen (make-hash-table :test 'perlnow-test))
         (difference ())
         (value)
         )
    (dolist (item list2)
      (puthash item t seen))
    (dolist (item list1)
      (unless (gethash item seen)
        (setq difference (cons item difference)))
      )
    difference
    ))
;;; end  efficient set subtraction (using a hash table)

;; ========
;; manipulate associated code files together

(defun perlnow-sync-save-run-string (run-string &optional harder-setting)
  "Save RUN-STRING value consistently in the appropriate locations.
Assigns the given string to the run-string of the current
buffer, and also of the \\[perlnow-associated-code] buffer.
Sets either \\[perlnow-run-string] or \\[perlnow-run-string-harder]
depending on the value of the given HARDER-SETTING."
  (if perlnow-trace (perlnow-message "Calling perlnow-sync-save-run-string"))
  (perlnow-save-run-string-hard-aware run-string harder-setting)
  (let* ((initial (current-buffer)))
    ;; (save-excursion ;; trust this not, to restore the original buffer (?)
    (perlnow-back-to-code)
    (perlnow-save-run-string-hard-aware run-string harder-setting)
    ;; )
    (switch-to-buffer initial)
    run-string))

(defun perlnow-save-run-string-hard-aware (run-string &optional harder-setting)
  "Save the RUN-STRING for the current buffer for the given HARDER-SETTING."
  (if perlnow-trace (perlnow-message "Calling perlnow-save-run-string-hard-aware"))
  (cond ( harder-setting
          (setq perlnow-run-string-harder run-string)
          )
        (t
         (setq perlnow-run-string run-string)
         )))

(defun perlnow-follow-associations-to-non-test-code (&optional filename)
  "Follow the chain of associations to a code file that is not a test.
Associations are defined by the \\[perlnow-associated-code]
buffer-local variable.  Begins checking the current buffer, or
the file FILENAME, if specified."
  (if perlnow-trace (perlnow-message "Calling perlnow-follow-associations-to-non-test-code"))
  (let* ((initial (current-buffer))
         (here (or filename (buffer-file-name)))
         (limit 100)
         (count 0)
         target
         )
    (unless here
      (error "Current buffer must have associated file, or you need to specify one."))
    (save-excursion
      (if here
          (find-file here))
      (let ((next here))
        (catch 'UP
          (while (not(perlnow-code-but-not-test-p))
            (progn
              (if (> (setq count (1+ count)) limit)
                  (throw 'UP nil))
              (setq next perlnow-associated-code)
              (find-file next)
              )))
        (setq target next))
      )
    (switch-to-buffer initial) ;; save-excursion doesn't always work
    target))

(defun perlnow-set-associated-code-pointers (there &optional here)
  "Make THERE the associated code for HERE (default: current buffer's file).
Revises the buffer-local variable \\[perlnow-associated-code] in
both locations. Note: expects that THERE will be an existing file.
Opens the file, if not open already. Both arguments should be full paths."
  (if perlnow-trace (perlnow-message "Calling perlnow-set-associated-code-pointers"))
  (let* ((initial (current-buffer))
         (here    (or here (buffer-file-name)))
         retval )
    (cond (perlnow-debug
           (message "a merrily asscoding we go...")
           (message "here: %s"  (pp-to-string here))
           (message "there: %s" (pp-to-string there))))
    (setq
     retval
     ;; here and there have to be full paths to files
     (cond ((not (and
                  there
                  here
                  (file-exists-p there)
                  (file-exists-p here)))
            (cond ((not (and there (stringp there)))
                   (message
                    (concat
                     "perlnow-set-associated-code-pointers: "
                     "'THERE' not a string, how can it be be a full file path?")) )
                  ((not (and there
                             (file-exists-p there)))
                   (message
                    (concat "perlnow-set-associated-code-pointers: "
                            "needs THERE to be path to existing file: "
                            (pp-to-string there)))) )
            (cond ((not (and here (stringp here)))
                   (message
                    (concat
                     "perlnow-set-associated-code-pointers: "
                     "'HERE' not a string, how can it be be a full file path?")) )
                  ((not (and here
                             (file-exists-p here)))
                   (message
                    (concat "perlnow-set-associated-code-pointers "
                            "needs HERE to be path to existing file: "
                            (pp-to-string here)))))
            nil)
           (t ;; valid arguments, so do it
            (find-file here)
            (setq perlnow-associated-code
                  (convert-standard-filename there) )
            (find-file there)
            (setq perlnow-associated-code
                  (convert-standard-filename here))
            )))
    (switch-to-buffer initial)
    (if perlnow-trace
        (message "   Returning from 'set-associated-code-pointers'"))
    retval))

(defun perlnow-generate-run-string-and-associate (program-file)
  "Generates a direct run-string for the perl PROGRAM-FILE.
This is used internally by routines such as \\[perlnow-guess-run-string].
This version is a convienience routine which also associates the
given PROGRAM-FILE with the current buffer's file."
  (if perlnow-trace (perlnow-message "Calling perlnow-generate-run-string-and-associate"))
  (let* ( (runstring "") )
    (setq runstring
          (perlnow-generate-run-string program-file))
    (perlnow-set-associated-code-pointers program-file)
    runstring))

;; TODO verify that program-file is code? (perlnow-perl-code-p file)
(defun perlnow-generate-run-string (program-file)
  "Generates a direct run-string for the perl PROGRAM-FILE.
This is used internally by routines such as \\[perlnow-guess-run-string].
If PROGRAM-FILE is nil or empty-string, this returns an empty-string."
  (if perlnow-trace (perlnow-message "Calling perlnow-generate-run-string"))
  (unless program-file (setq program-file ""))
  (let* ((perl-command (perlnow-how-to-perl))
         runstring)
    ;; If we've got a pf it'S just "perl pf", if not it's just "" *not* "perl ".
    (cond ((and program-file (not (string= program-file "")))
           (setq runstring
                 (concat perl-command " " ;; can't quote perl-command, handles "perl -T" wrong.
                         (shell-quote-argument program-file))))
          (t
           (setq runstring "")))
    runstring))




;; This actually WORKED.  (I've lost track of The number of hacks
;; I tried that really should've worked... yeah, ok, this is the Right Way,
;; but seriously there is something fundamentally squirrelly about the
;; behavior of next-error, only a fool would think about tweaking it.)
(defun perlnow-associate-last-with-this ()
 "Associate the last buffer file name with the current buffers.
Uses global `perlnow-last-buffer-file-name' to get the value of
the last one.
Intended to be used via hooks such as `next-error-hook'."
 (let* ((there perlnow-last-buffer-file-name)
        (here (buffer-file-name)) )
   (cond ((and
           (perlnow-perl-code-p there)
           (perlnow-perl-code-p here))
          (perlnow-set-associated-code-pointers there here)
          (if perlnow-debug
              (message
               "perlnow-associate-last-with-this:\nhere: %s\n there: %s\n" here there))
          ))))

;; (remove-hook 'next-error-hook 'perlnow-associate-last-with-this)
(add-hook 'next-error-hook 'perlnow-associate-last-with-this)



;;=======
;; prompt functions
(defun perlnow-prompt-for-module-to-create (where what)
  "Internally used by \\[perlnow-module-two-questions\] to ask the two questions.
Asks for the WHERE, i.e. the \"module root\" location, and the WHAT, the name
of the perl module to create there.  Checks to see if one exists already,
and if so, asks for another name.  The location defaults to the current
`default-directory'.  Returns a two element list, location and package-name.\n
Note: This is all used only by the mildly deprecated \\[perlnow-module-two-questions\]."
  (interactive "DLocation for new module?  \nsName of new module \(e.g. New::Module\)? ")
  (if perlnow-trace (perlnow-message "Calling perlnow-prompt-for-module-to-create"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-prompt-for-cpan-style"))
  (let ( staging-area
         )
    (setq staging-area (perlnow-staging-area where what))
    (while (file-exists-p staging-area)  ; really, directory exists
      (setq where-and-what  ; that's a list: (dev-location package-name)
            (call-interactively 'perlnow-prompt-for-cpan-style-again))
      (setq where (car  where-and-what))
      (setq what  (cadr where-and-what))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-prompt-for-cpan-style-again"))
  (list where what))

(defun perlnow-prompt-user-for-file-to-create (ask-mess default-location)
  "Ask for the name of the file to create.
Check to see if one exists already, and if so, ask for another name.
Asks the question ASK-MESS, and defaults to the using the location
DEFAULT-LOCATION.  Returns a list of a single string, full file name
with path."
  (if perlnow-trace (perlnow-message "Calling perlnow-prompt-user-for-file-to-create"))
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
;; end  prompt functions

;;========
;; file creation

(defun perlnow-make-sure-file-exists ()
  "Forcibly save the current buffer to it's associated file.
This is to make sure that the file actually exists."
  (if perlnow-trace (perlnow-message "Calling perlnow-make-sure-file-exists"))
  (set-buffer-modified-p t)
  (save-buffer))

(defun perlnow-change-mode-to-executable ()
  "Make the file associated with the current buffer executable."
  (if perlnow-trace (perlnow-message "Calling perlnow-change-mode-to-executable"))
  (perlnow-make-sure-file-exists)
  (let* (
;;         (all-but-execute-mask ?\666)
         (all-but-execute-mask #o666)  ;; Tue  March 28, 2017  22:39
         (filename (buffer-file-name))
         (file-permissions (file-modes filename))
         (new-file-permissions
          (+ (logand file-permissions all-but-execute-mask)
             perlnow-executable-setting)
          ))
    (set-file-modes filename new-file-permissions)))

(defun perlnow-create-with-template (filename template &optional force)
  "Create a new file with a template.el template.
Given FILENAME and TEMPLATE this does the actual creation of the
file and associated buffer using the template.  As a side-effect,
it sets the global `template-file' here.  Returns t on
success (i.e. checks that file exists).  If either the FORCE
option or the `perlnow-quiet' variable is set, creates any needed
directories silently, and moves existing files out of the way,
renaming with an '.OLD' suffix."
  (if perlnow-trace (perlnow-message "Calling perlnow-create-with-template"))
  (if perlnow-debug
      (message "\"create w template\" %s %s" filename template ))
  ;; override local 'force' setting with global (this silences questions)
  (if perlnow-quiet (setq force perlnow-quiet))

  (unless perlnow-perl-sub-name
    (setq perlnow-perl-sub-name ""))

  (let* ((backup-name (concat filename ".OLD"))
         (loc (file-name-directory filename))
         )
    (cond (force
           ;; (message "perlnow-create-with-template is using force")
           (cond ((file-exists-p filename)
                  ;; (message "perlnow-create-with-template renaming file: %s" filename)
                  (rename-file filename backup-name t)
                  ))
           (cond ((not (file-directory-p loc))
                  ;; (message "perlnow-create-with-template creating dir: %s " loc)
                   (make-directory loc t))
                 )
           ))
    ;; (message "perlnow-create-with-template: template %s in template-new-file" template)

    ;; The "template-file" must be set here because of a bug in
    ;; template.el, when using template-new-file non-interactively.
    (setq template-file (template-split-filename filename))
    (template-new-file filename template)

     (write-file filename)
     (if (perlnow-perl-code-p filename)
         (funcall (perlnow-lookup-preferred-perl-mode)))
    (file-exists-p filename) ;; return t on success
    ))

;; end file creation

;;========
;;  dynamically set dev/lib/bin

(defun perlnow-XXX ( &optional md )
  ""
  (if perlnow-trace (perlnow-message "Calling XXX"))
  (unless md (setq md perlnow-metadata))

  (let* (
         (testloc          (nth 0  md))
         (dotdef           (nth 1  md))
         (namestyle        (nth 2  md))
         (testloc-absolute (nth 3  md))
         (hyphenized       (nth 4  md))
         (package-name     (nth 5  md))
         (incspot          (nth 6  md))
         (buffer           (nth 7  md))
         (filename         (nth 8  md))
         (fileloc          (nth 9  md))
         (basename         (nth 10 md))
         (context          (nth 10 md))
         ;; (sub-name         (nth 11 md))

         (slash perlnow-slash)

         dev lib bin t-loc
         )

    (cond ((string= context "module")
           (setq dev nil)
           (setq lib incspot)
           (setq bin nil)
           (setq t-loc nil) ;; testloc-absolute
           )
          ((string= context "object")
           (setq dev nil)
           (setq lib incspot)
           (setq bin nil)
           (setq t-loc nil) ;; testloc-absolute
           )
          ((string= context "cpan")
           (setq dev (perlnow-fixdir (concat incspot slash ".." slash)))
           (setq lib incspot)
           (setq bin (concat slash "script" slash)) ;; good default, but should check it
           (setq t-loc (concat slash "t" slash)) ;; testloc-absolute !
           )
          ((string= context "script")
           (setq dev nil)
           (setq lib nil)
           (setq bin fileloc)
           (setq t-loc nil)
           )
          ((string= context "test")
           (setq dev nil)
           (setq lib incspot) ;; metadata calls incspot-from-t
           (setq bin fileloc)
           (setq t-loc nil)
           )
          ;;         ((string= context '')
          ;;          )
          (t
           (setq dev nil)
           (setq lib nil)
           (setq bin nil)
           (setq t-loc nil)
           )
          )
    (list dev lib bin t-loc)
 ))


;;========
;; buffer/file probes (determine what kind of code it is, etc)

(defun perlnow-nix-script-p ()
  "Determine if the buffer looks like a 'nix style executable script.
Looks for the hash-bang line at the top."
  (if perlnow-trace (perlnow-message "Calling perlnow-nix-script-p"))
  (save-excursion
    (let ( (hash-bang-line-pat "^[ \t]*#!") )
      (goto-char (point-min))
      (looking-at hash-bang-line-pat)
      )))

(defun perlnow-script-p (&optional file)
  "Determine if the buffer looks like a perl script.
If FILE is given, opens that first.
This assumes we have a perl script if there's a
perl hashbang line *or* if it is in a perl mode,
and also verifires that it's not a module."
  (if perlnow-trace (perlnow-message "Calling perlnow-script-p"))
  (let ((initial-buffer (current-buffer))
        retval
        )
    (cond (file
           (find-file file))
          (t
           (setq file (buffer-file-name))
           ))
    (save-excursion
      (let (;; The following assumes perl is called something like perl
            (hash-bang-line-pat "^[ \t]*#!.*perl")
            )
        (setq retval
              (or
               (progn
                 (goto-char (point-min))
                 (looking-at hash-bang-line-pat))
               (and
                ;; (string-match perl-mode-pat mode)
                (perlnow-perl-mode-p file)
                (not (perlnow-module-code-p))
                (not (perlnow-test-p file))
                )))
        ))
    (switch-to-buffer initial-buffer)
    retval))

(defun perlnow-perl-mode-p (&optional file)
  "Does the current buffer's major mode look like a perl mode?
This will return non-nil for \"sepia-mode\" as well as \"perl-mode\"
and \"cperl-mode\", but not for \"perlnow-select-mode\".
If FILE is given, opens that first."
  (if perlnow-trace (perlnow-message "Calling perlnow-perl-mode-p"))
  (let ((initial-buffer (current-buffer)) )
    (cond (file
           (find-file file))
          (t
           (setq file (buffer-file-name))
           ))
    (let* (;; matches cperl-mode, perl-mode, or sepia-mode, and *not* perlnow-select-mode
           (perl-mode-pat "^\\\(sepia\\\|cperl\\\|perl\\\)-mode$")
           (mode (pp-to-string major-mode))
           (retval (string-match perl-mode-pat mode))
           )
      (switch-to-buffer initial-buffer) ;; save-excursion doesn't always work
      retval)))

(defun perlnow-test-p (&optional file)
  "Determine if FILE looks like a perl test, defaulting to the current buffer.
Looks for a *.t extension on file name, then looks for a 'use Test::' line."
  (if perlnow-trace (perlnow-message "Calling perlnow-script-p"))
  (let* ((retval  nil)
         (use-test-pattern "\\b\\(require\\|use\\)[ \t]+Test::\\b")
         (initial-buffer (current-buffer))
         (initial-point  (point))
         )
    (save-excursion
      (cond((not file)
            (setq file (buffer-file-name)))
           (t
            (find-file file)))

      (cond ((and file (string-match "\\\.t$" file))
             (goto-char (point-min))
             (if (re-search-forward use-test-pattern nil t)
                 (setq retval t))
             ))
      )
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    retval))

(defun perlnow-module-code-p (&optional file)
  "Determine if the buffer looks like a perl module.
If given FILE, opens that first.  This looks for the package line
near the top, and checks for a file extension of \"pm\".
Note: it's often more useful to just try to get the package
name directly: \\[perlnow-get-package-name-from-module-buffer]."
  (if perlnow-trace (perlnow-message "Calling perlnow-module-code-p"))
  (let* ((initial-buffer (current-buffer))
         ext  package-name
         )
      (cond (file (find-file file)
             )
            (t
             (setq file (buffer-file-name))
             ))
      (if file
          (setq ext (file-name-extension file)))
      (if (string= ext "pm")
          (setq package-name (perlnow-get-package-name-from-module-buffer)))
      (switch-to-buffer initial-buffer)
      package-name))

(defun perlnow-exporter-code-p (&optional file)
  "Return t if the current buffer looks like an Exporter-based module.
Return nil otherwise."
  (if perlnow-trace (perlnow-message "Calling perlnow-exporter-code-p"))
  (let ((initial-buffer (current-buffer))
        (initial-point  (point))
        ;; searches for "use Exporter" or "require Exporter"
        (exporter-pattern "\\b\\(require\\|use\\)[ \t]+Exporter\\b")
        retval )
    (cond (file
           (find-file file))
          (t
           (setq file (buffer-file-name))
           ))
    (goto-char (point-min))
    (setq retval
          (re-search-forward exporter-pattern nil t))
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    retval))

(defun perlnow-cpan-style-code-p ()
  "Determine if this file looks like it's in a cpan-style dev tree.
Returns the staging-area name, or nil if not found.
From the current file-buffer, climbs upwards, looking for a level
with a Makefile.PL or a Build.PL."
  (if perlnow-trace (perlnow-message "Calling perlnow-cpan-style-code-p"))
  ;; as written, this also checks for a "lib" or "t" dir next to it.
  (let* ( (staging-area (perlnow-find-cpan-style-staging-area) )
          )
    staging-area))

(defun perlnow-perl-code-p (&optional file)
  "Return t if FILE seems to be perl code, defaults to current-buffer.
Checks for the usual perl file extensions, and if need
be opens the file to look for a package line or a hashbang line."
  (if perlnow-trace (perlnow-message "Calling perlnow-perl-code-p"))
  (let ((initial (current-buffer))
        retval
        )
    (cond (file
           (find-file file))
          (t
           (setq file (buffer-file-name))
           ))
    (cond ((not (perlnow-perlish-true-p file)) ;; non-nil and not empty string
           (setq retval nil))
          ((setq retval
                 (or
                  (string-match "\.t$"  file)   ;; TODO but is a *.t always perl?
                  (string-match "\.pm$"  file)
                  (string-match "\.pl$"  file)
                  )))
          (t
           (setq retval
                 (or
                  (perlnow-module-code-p)
                  (perlnow-script-p)
                  ;; This is used to check if a perl mode should be enabled,
                  ;; so we can't check this here
                  ;; (perlnow-perl-mode-p)
                  ))))
    (switch-to-buffer initial) ;; save-excursion doesn't always work
    retval))

(defun perlnow-code-but-not-test-p (&optional here)
  "Is HERE (default: current buffer) a perl file that is not a test?"
  (if perlnow-trace (perlnow-message "Calling perlnow-code-but-not-test-p"))
  (let* ((initial (current-buffer))
         (test-file-p)
         (here)
         (retval)
         )
    (save-excursion
      ;; if passed as argument, might not be open yet
      (cond (here
             (find-file here))
            (t
             (setq here (buffer-file-name))))
      (if here
          (setq test-file-p (string-match "\.t$"  here)))
      (setq retval
            (and
             (not test-file-p)
             ;; (perlnow-perl-code-p file-name)
             (perlnow-perl-mode-p) ;; TODO could be more thorough, but tis late
             ))
      )
    (switch-to-buffer initial) ;; save-excursion doesn't always work
    retval
    ))

(defun perlnow-module-file-p (&optional file)
  "Determine if the FILE looks like a perl module."
  (if perlnow-trace (perlnow-message "Calling perlnow-module-file-p"))
  (unless file
    (setq file (buffer-file-name)))
  (let (retval)
    (setq retval
          (cond ((not file) nil) ;; if file is nil, it ain't a module
                ((string-match "\.pm$"  file)) ;; right extension: pass
                ((file-exists-p file)
                 (save-excursion
                   (find-file file)
                   (perlnow-module-code-p))) ;; has "package" line: pass
                (t
                 nil)))
    retval))

(defun perlnow-script-file-p (&optional file)
  "Determine if the FILE looks like a perl script."
  (if perlnow-trace (perlnow-message "Calling perlnow-script-file-p"))
  (unless file
    (setq file (buffer-file-name)))
  (let ((retval
         (cond ((not file) nil) ;; if file is nil, it ain't a script
               ((string-match "\.t$\\|\.pl$"  file)) ;; good extension: pass
               ((file-exists-p file)
                (save-excursion
                  (find-file file)
                  (perlnow-script-p))) ;; pass: hashbang || perl mode && no package line
               (t
                nil))))
    retval))

(defun perlnow-test-select-menu-p ()
  "Identify whether the current buffer looks like a test select menu.
Checks mode and buffer name."
;; TODO could also look for lines with text property perlnow-file-path
  (let* ( (curr-mode-name     mode-name)   ;; "perlnow-select"
          (curr-mode-symbol   major-mode)  ;; ‘perlnow-select-mode’
          (curr-buff-name (buffer-name) ) ;; *select test file*
          retval
          )
    (setq retval
          (cond(
                (or
                 (string= curr-mode-name   "perlnow-select")
                 (equal   curr-mode-symbol 'perlnow-select-mode)
                 (string= curr-buff-name    perlnow-select-test-file-buffer-name)
                 )
                t)
               (t
                nil)))
    retval))

;; end buffer probes

;;========
;; buffer scraping -- get metadata about the code buffer

;; An example of typical code to get and unpack metadata
;; using the following routine:

;; (defun bloorg ()
;;   ""
;;   (let* (
;;          (md (perlnow-metadata))

;;          (testloc          (nth 0  md))
;;          (dotdef           (nth 1  md))
;;          (namestyle        (nth 2  md))
;;          (testloc-absolute (nth 3  md))
;;          (hyphenized       (nth 4  md))
;;          (package-name     (nth 5  md))
;;          (incspot          (nth 6  md))
;;          (buffer           (nth 7  md))
;;          (filename         (nth 8  md))
;;          (fileloc          (nth 9  md))
;;          (basename         (nth 10 md))
;;          (project-type     (nth 11 md))
;;          (file-type        (nth 12 md))
;;          (sub-name         (nth 13 md))
;;          )
;;     ;; ...
;;     ))

;; TODO
;; At present, this is just used by perlnow-test-create and
;; perlnow-test-create-manually.  Refactor to use more widely?
(defun perlnow-metadata (&optional file-name)
  "Tries to give you \"metadata\" for the relevant perl code.
Based on your present context (as inferred from the current buffer,
of the FILE-NAME if given), it determines various pieces of
information often needed by different perlnow functions.

If a module file is open, it tells information about that module,
if a test file is open, it tells you about the test file, but
also tries to determine the module being tested by the code, and
provides some information about that.  The case of a script
file is similar to that of the test file \(thought at present,
not as well supported\).

If a \"*select test file*\" buffer, it again tries to tell you
something about the module it figures you're testing, making
inferences based on the current line.

The metadata is returned as an ordered list.

An example of returned metadata.

                     testloc:  ../t
                      dotdef:  incspot
                   namestyle:  fullauto
            testloc-absolute:  /home/doom/t/
     hyphenized-package-name:  Skank-Mama
                package-name:  Skank::Mama
                    incspot:  /home/doom/lib/
                      buffer:  #<buffer Mama.pm>
                   file-name:  /home/doom/lib/Skank/Mama.pm
               file-location:  /home/doom/lib/Skank/
                    basename:  Mama
                   file-type:  object
                project-type:  cpan
                    sub-name:  clean_up

\"project-type\" is a string code which at present
just \"cpan\" or \"noncpan\": it tells you something
about the larger context of the current file.

\"file-type\" is a string code which may be:
  module  object script  test  test-select-menu  man-page unknown

Note: \"test-select-menu\" and \"man-page\" are strictly a
file-types, but rather buffer-types: this might be thought of
as \"calling-context\".  It indicates something about the active
buffer when metadata was called:

  module             an exporter-based module  (TODO: module-exporter?)
  object             a oop-based module        (TODO: module-object?)
  script             a script
  test               a test file
  test-select-menu   menu of test files
  man-page           man page for a perl module
  unknown            something else

"
  (if perlnow-trace (perlnow-message "Calling perlnow-metadata"))
  (let ((initial-point  (point))
        (initial-buffer (current-buffer))
        md-list
        testloc  dotdef  namestyle
        testloc-absolute
        package-name  incspot  hyphenized-package-name
        buffer
        file-name  file-location  basename
        policy-metadata
        file-type project-type
        sub-name

        staging-area

        script-name
        test-name

        associated-module-file
        associated-module-package-name
        associated-module-hyphenized
        )
    (cond (file-name
           (find-file file-name)
           ))

   ;; presuming by buffer we mean the calling context buffer (same as initial-buffer)
   ;; TODO maybe trace asscode links back from non-file buffers to first code?
   (setq buffer (current-buffer))

   ;; Now let's define file-context and project-context
   (setq file-type     "unknown")
   (setq project-type  "noncpan")
   (cond ((perlnow-cpan-style-code-p)  ;; works even in a test select menu buffer ((man-page, too? TODO))
          (setq project-type "cpan")))
   (cond ((setq package-name (perlnow-module-code-p))
          (cond ((perlnow-exporter-code-p)
                 (setq file-type "module"))
                (t
                 (setq file-type "object"))))
         ((perlnow-script-p)
          (setq file-type "script"))
         ((perlnow-test-select-menu-p)
          (setq file-type "test-select-menu"))
         ((perlnow-test-p)
          (setq file-type "test"))
         ;; (;; Man or WoMan buffer  ;; TODO Q: does this cause problems her?
         ;;     (setq package-name (perlnow-get-package-name-from-man))
         ;;     (setq file-type "man-page"))
         )
   ;; package-name set already for module and object and maybe man-page
   ;; TODO better to do the others here?  Make a guess for test and script

   ;; now do the test policy settings (may need later in this function)
   ;; We default to a "module" test policy, and override later as appropriate,
   ;; but really, the "overrides" are noops *except* for script, with it's dotdef='fileloc'
   (setq testloc   perlnow-test-policy-test-location-module  )
   (setq dotdef    perlnow-test-policy-dot-definition-module )
   (setq namestyle perlnow-test-policy-naming-style-module   )
   (cond
    ((string= project-type "cpan")
     (setq testloc   perlnow-test-policy-test-location-cpan  )
     (setq dotdef    perlnow-test-policy-dot-definition-cpan )
     (setq namestyle perlnow-test-policy-naming-style-cpan   )
     )
    (
     (string= file-type "module")
     ;; nothing to do: we already used the module values as a default
     )
    ((perlnow-script-p) ;; TODO do trial runs some time (ntigas)
     (setq testloc   perlnow-test-policy-test-location-script  )
     (setq dotdef    perlnow-test-policy-dot-definition-script )
     (setq namestyle perlnow-test-policy-naming-style-script   )
     )
    ((string= file-type "test-select-menu")
     ;; TODO for now, just treat this case like a module ((really: hope policy dies))
     )
    ((string= file-type "test")
     ;; TODO need to know if this is a script or module test
     )
    (t ;; other (whatever that would be...)
     (setq testloc    "../t" )
     (setq dotdef     "incspot")
     (setq namestyle  "numeric")
     )) ;; end policy
   (setq test-policy-trio (list testloc dotdef namestyle))
   (cond
     ((string= file-type "test-select-menu")
      ;; get the module name from the test file name
      (let* ((selected-file-compact (perlnow-select-file-from-current-line))
             (path (perlnow-get-path-from-markedup-name selected-file-compact))
             (testfile (concat path selected-file-compact)))
        (setq package-name (perlnow-module-from-t-file testfile t))
        (setq testloc-absolute (perlnow-t-dir-from-t testfile))
        (perlnow-incspot-from-t testfile test-policy-trio)

        (setq file-name (perlnow-full-path-to-module incspot package-name))
        (setq file-location file-name)
        (setq basename (file-name-sans-extension (file-name-nondirectory file-name)))

        ;; the select menu buffer, which is literally the current buffer.  TODO okay?
        (setq buffer    (current-buffer))  ;; TODO factor out above.  there's always a buffer.
        )) ;; end test-select-menu
;; TODO this was causing problems... why exactly?
;;      (;; Man or WoMan buffer
;;       (setq package-name (perlnow-get-package-name-from-man))
;;       (setq file-type "man-page")
;;        )
     (t ;; not a test-select-menu or man page: assuming it's a basic file-buffer
      ;; We do all of the following for every file buffer
      ;; First, the basic file information
      (cond ((setq file-name     (buffer-file-name))
             (setq file-location (file-name-directory file-name))
             (setq buffer        (current-buffer)) ;; TODO factor out above.  there's always a buffer.
             (setq basename      (file-name-sans-extension (file-name-nondirectory file-name)))
             ))
      ;; TODO I think: for every block, check whether we're cpan-style?
      ;;      OR: a separate block first, if cpan-style, skip the others...

      (cond
;;        (;; if cpan-style
;;         (string= project-type "cpan")
;;         ;; maybe?
;;        )

       (;; if module
        (or (string= file-type "module") (string= file-type "object"))
        (setq package-name (perlnow-get-package-name-from-module-buffer))
        (setq sub-name (or (perlnow-sub-at-point) ""))
        (setq testloc-absolute
              (perlnow-testloc-from-policy testloc dotdef namestyle))
        (if perlnow-debug
            (message " package-name: %s file-location: %s"   package-name file-location))
        (setq incspot (perlnow-get-incspot package-name file-location))
        )
       ((string= file-type "test")
        (let* (
               (path (file-name-directory file-name))
               (testfile file-name)
               ;; (hyphenized (perlnow-extract-hyphenized-from-standard-t-name testfile))

               (fields (perlnow-parse-standard-t-name testfile))
               (prefix      (nth 0 fields))
               (hyphenized  (nth 1 fields))
               (subname     (nth 2 fields))
               (description (nth 3 fields))

               (colonized (replace-regexp-in-string "-" "::" hyphenized))
               )
          (setq package-name colonized)
          (setq testloc-absolute (perlnow-t-dir-from-t testfile))
          (setq incspot (perlnow-incspot-from-t testfile test-policy-trio))
          ;; (setq sub-name (or (perlnow-sub-at-point) ""))
          (setq sub-name subname)

          ;; TODO Question though: if in the select buffer, I'd return info about the related module file.
          ;; Should I do that here with the *.t file?   Could be... multiple fields, code-* and test-*?
          )
         ;;;; TODO what about from a test created from a script?
         ;;;;      can you trace from that script to a module?
        )
       ((string= file-type "script")
        (setq sub-name (or (perlnow-sub-at-point) ""))
        (setq testloc-absolute (perlnow-testloc-from-policy testloc dotdef namestyle))
        (cond (perlnow-associated-code
               (let* ( (candidate (perlnow-follow-associations-to-non-test-code))
                       (ext (file-name-extension candidate))
                       )
                 (unless ext (setq ext ""))
                 (cond ((string-match "pm$" ext)
                        (setq package-name
                              (perlnow-get-package-name-from-module-buffer candidate))
                        (setq incspot (perlnow-get-incspot package-name candidate))
                        )))))
        (unless package-name
          ;; TODO move/copy this sort of thing to a context='cpan' block?
          (cond ((setq staging-area (perlnow-find-cpan-style-staging-area))
                 (setq incspot staging-area)
                 (let* (
                        ;; remove trailing slash so file-name-directory & nondirectory can work
                        (last-slash-pat (concat perlnow-slash "$"))
                        (incspot-trimmed
                         (replace-regexp-in-string last-slash-pat "" incspot))
                        (incspot-path      (file-name-directory    incspot-trimmed))
                        (incspot-sans-path (file-name-nondirectory incspot-trimmed))
                        )
                   (setq hyphenized-package-name incspot-sans-path)
                   (setq package-name (mapconcat 'identity (split-string hyphenized-package-name "-") "::"))

                   ;; TODO this is location of cpan-style staging-area.  Return? (always oneup from incspot)
                   ;;    incspot-path

                   ))))
        ;; TODO if package-name might not be defined if script has no asscode and is non-cpan.
        ;;      any way to handle?   (unless package-name ...
        ;; TODONT hypothetically, could scrape script for likely-looking "use"
        ;;        e.g. one that has an open buffer, a mention as a recent pick, etc.
        ))))
   (cond ((and package-name (not hyphenized-package-name))
          (setq hyphenized-package-name
                (mapconcat 'identity (split-string package-name "::") "-"))
          ))
   (perlnow-stash-put testloc-absolute incspot) ;; very important side-effect.  move elsewhere?
   (setq md-list
         (list testloc dotdef namestyle
               testloc-absolute
               hyphenized-package-name package-name incspot
               buffer file-name file-location basename
               file-type
               project-type
               sub-name
               ))
    (if perlnow-debug (perlnow-report-metadata md-list))
    ;; returning from any excursions
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    md-list))


;;------
;; buffer scraping -- extracting info from code buffer
(defun perlnow-hashbang ()
  "What is the hash bang line for this file buffer?
Returns nil if there is none."
  (if perlnow-trace (perlnow-message "Calling perlnow-hashbang"))
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
      (if (looking-at hash-bang-pat)
          (setq return
                (match-string 1)))
      )))

(defun perlnow-sub-at-point ()
 "Returns the name of the current perl sub, or nil if there is none.
When run inside an open buffer of perl code.  It tries to find
the name of the current perl sub \(the one that the cursor is
either inside of, or just in front of\).  Returns nil on failure,
sub name on success."
  (if perlnow-trace (perlnow-message "Calling perlnow-sub-at-point"))
;; Algorithm:  save initial location
;;             skip back to previous ^sub.  scrape name
;;               if that's nil, then skip forward, scrape name: done
;;             skip to close of sub: check that location.
;;               if this is *after* the initial location, we've got our sub
;;               if this is *before* the inital location, we were between subs
;;                  skip forward to next ^sub.  scrape name.
 (let* ((initial-point (point))
        (open-brace-pat "[\\{]")          ;; open curly brace (TODO optimize?)
        subname return)
   (save-excursion
     (setq return
         (catch 'IT
           ;; (setq subname (perlnow-previous-sub))
           (setq subname (perlnow-find-sub -1)) ;; previous
           (cond ((not subname)
                  ;; (setq subname (perlnow-forward-sub))
                  (setq subname (perlnow-find-sub 1))
                  (throw 'IT subname)
                  ))

           (re-search-forward open-brace-pat nil t)
           (backward-char 1)
           (forward-sexp 1)
           (if (> (point) initial-point)
               (throw 'IT subname))
           ;; (setq subname (perlnow-forward-sub))))
           (setq subname (perlnow-find-sub 1))))
   return)))

(defun perlnow-find-sub ( &optional direction )
 "Looks for nearest sub definition, and returns the name of sub.
If DIRECTION is -1 it will search backward. The default is to
search forward (DIRECTION is nil or +1)."
  (if perlnow-trace (perlnow-message "Calling perlnow-find-sub"))
 (let* ((sub-begin-pat "^[ \t]*sub ")     ;; perl "sub" keyword at bol
        (after-subname-pat "[ \\\\(\\{]") ;; either paren or curly, after space
        (open-brace-pat "[\\{]")          ;; open curly brace (TODO optimize?)
        current-word return)
   (unless direction
     (setq direction 1))
      ;;;; first we will skip forward to start of sub
      ;; if we're *on top* of the keyword "sub", move backward so regexp works
      (forward-word 1) (backward-word 1) ;; twiddle to position at start of word
      (setq current-word (thing-at-point 'word))
      (cond ((string= current-word "sub")
             (backward-word 1)
             ))
      (setq return
            (catch 'OUT
              (unless (re-search-forward sub-begin-pat nil t direction)
                (throw 'OUT nil))
              ;; skip past whitespace to start of name
              (cond ((= direction -1)
                     (forward-word 1)))
              (forward-word 1)
              (backward-word 1)

              (let ((beg (point)))
                (unless (re-search-forward after-subname-pat nil t)
                  (throw 'OUT nil))
                (backward-word 1)
                (forward-word 1)
                (setq return
                      (buffer-substring-no-properties beg (point)))
                )))
    return))

;; Used by perlnow-script-using-this-module (via perlnow-do-script-from-module)
;; perlnow-edit-test-file (via perlnow-open-test-file)
;;   and ... fullauto ... to make testing easier
(defun perlnow-sub-name-to-var ()
  "Assigns the current perl sub name to `perlnow-perl-sub-name'.
It is then available as the \(>>>PERL_SUB_NAME<<<\) template expansion.
This is intended to be run inside an open buffer of perl code.
The \"current sub\" is as determined by \\[perlnow-sub-at-point].
Returns nil on failure, sub name on success."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-sub-name-to-var"))
  (let ((sub-name (or (perlnow-sub-at-point) "")))
    (setq perlnow-perl-sub-name sub-name)))

;;;--------
;; perlnow-revise-export-list and related functions
;;
;; At present: the following assumes a layout like this:
;;
;;   our %EXPORT_TAGS = ( 'all' => [
;;     # names of items to export
;;     qw(
;;        nada
;;        slackoff
;;       ) ] );
;;   # The above allows declaration	use Modular::Stuff ':all';
;;
;;   our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
;;   our @EXPORT = qw(  ); # items to export into callers namespace by default.
;;                         # (don't use this without a very good reason.)
;;
(defun perlnow-list-all-exported-symbols ()
  "Extracts the exported symbols."
  (if perlnow-trace (perlnow-message "Calling perlnow-list-all-exported-symbols"))
  (unless (perlnow-module-code-p)
    (error "perlnow-list-all-subs expects to be called from a module buffer."))
  (let* ((original-case-fold case-fold-search)
         (export-pattern "EXPORT[ \t]+=")
         (open-quoted-words-pattern
          "qw(")
         (closing-quoted-words-pattern
          ")")
         beg end export-string-1 export-string-2 export-list
         )
    (setq case-fold-search nil)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "EXPORT_TAGS" nil t)
      (re-search-forward open-quoted-words-pattern nil t)
      (setq beg (+ (point) 1))
      (re-search-forward closing-quoted-words-pattern nil t)
      (setq end (- (point) 1))
      (setq export-string-1 (buffer-substring-no-properties beg end))

      ;;      (re-search-forward export-pattern nil t) ;; skip EXPORT_OK one

      (re-search-forward export-pattern nil t)
      (re-search-forward open-quoted-words-pattern nil t)
      (setq beg (+ (point) 1))
      (re-search-forward closing-quoted-words-pattern nil t)
      (setq end (- (point) 1))
      (setq export-string-2 (buffer-substring-no-properties beg end))
      )

    ;; split export-string-1 && export-string-2 on whitespace (including newlines)
    (setq export-list
          (append
           (split-string export-string-1)
           (split-string export-string-2))
          )
    (setq case-fold-search original-case-fold)
    export-list
    ))

(defun perlnow-list-all-exported-symbols-report ()
  "Echoes output of \\[perlnow-list-all-exported-symbols] via message."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-list-all-exported-symbols-report"))
  (let* ( (list (perlnow-list-all-exported-symbols) )
          )
    (message "%s" list)
    ))
;; end  implemented for perlnow-revise-export-list

;;;--------
;;; package-name scraping functions

;; Not in use: typically need to know if it came from a code
;; buffer or a man page.
(defun perlnow-get-package-name ()
  "Return the module name  \(in perl's double colon separated form\)
from either a module buffer or a Man page showing the perldoc for it,
or nil if none is found."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-package-name"))
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

(defun perlnow-get-package-name-from-module-buffer (&optional file-name)
  "Get the module name from the first package line.
This will be in perl's double colon separated form, or it will
return nil if none is found.  If a FILE-NAME is given, will open
that file, otherwise tries to work on the current buffer.
Note: this is often used to determine if the current buffer
looks like a perl module."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-package-name-from-module-buffer"))
  (save-excursion
    (if file-name
        (find-file file-name))
    (let* ((package-line-pat "^[ \t]*package[ \t]+\\(.*?\\)[ \t;]")
           ;; captures "Module::Name"
           (module-name nil)
           (line-count 1)
           (line-limit 24)
           moved   ;; if not moved, we're at eof
           )
      (goto-char (point-min))
      ;; repeat ... untill
      (while (progn
               (if (looking-at package-line-pat)
                   (setq module-name (match-string-no-properties 1)))
               (setq moved (forward-line 1))
               (setq line-count (1+ line-count))
               ;; until we've got the name, gone too far, or at the end
               (not (or
                     module-name
                     (> line-count line-limit)
                     (not moved)))
               ))
      module-name)))

(defun perlnow-get-package-name-from-man ()
  "Return the module name from a man page buffer displaying the perldoc.
If not a man page buffer, returns nil.  This version is fairly
simple to avoid returning false positives."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-package-name-from-man"))
  (save-excursion
    (let ((buffer-name-string (buffer-name))
          return   buffer-name-string   candidate-list  candidate
          )
      (cond (buffer-name-string
             (goto-char (point-min))
             (cond ((string-match "\\*\\(Wo\\)*Man \\(.*\\)\\*$" buffer-name-string)
                    (setq candidate (match-string 2 buffer-name-string))
                    ))))
      candidate)))

;;;---------
;;; perlnow-module-from-t-file (package name scraping continued)

;; TODO
;; There may be fancier ways of infering the module a test file is
;; targeting if my naming convention isn't in use, using info such as:
;;   o  inside the *.t, there must be a use/use_ok of the
;;      module being tested (along with others, of course)
;;   o  Going from the t-directory back to the incspot,
;;      you can get a list of (possibly) associated modules.
;;    => If there's only a single module overlap, that's the logical pick
;;
;; And if none of this works, you can use the select menu's
;; associated context (but that should be handled by the callling defun)

(defun perlnow-module-from-t-file ( &optional t-file colonized-flag )
  "Try to infer an associated module given a test file, T-FILE.
If not provided as an argument, the T-FILE will be read from
near point, presuming a \"*select test file*\" menu is the
current buffer.  Return defaults to \"hyphenized\" form, but
the COLONIZED-FLAG option can be set to request double-colon separators."
  (cond ((not t-file)
         (let*
             ((selected-file-compact (perlnow-select-file-from-current-line))
              (path (perlnow-get-path-from-markedup-name selected-file-compact))
              (t-file (concat path selected-file-compact))
              )))) ;; end cond not t-file

  (let ( colonized  hyphenized  fields  ret )
    (cond ((setq fields (perlnow-parse-standard-t-name t-file))
           (setq hyphenized (nth 1 fields))
           (setq colonized (replace-regexp-in-string "-" "::" hyphenized))
           )
          ((setq colonized (perlnow-get-package-name-from-useok t-file))
           (setq hyphenized (replace-regexp-in-string "::" "-" colonized))
           )
          ;; TODO try still other methods of scraping module info?
          (t
           (message "Could not determine module from t-file.")
           )
          )
    (setq ret (cond (colonized-flag colonized) (t hyphenized)))
    ret))


(defun perlnow-get-package-name-from-useok ( &optional t-file )
  "Get the module package name found in a use_ok line.
A perl test file often has a uses \"use_ok\" rather than \"use\",
and that can be a good hint about which module the test file
is primarily intended to exercise.  Example:

    use_ok( 'WildSide::Glam' ,  qw(sashay) );

When there's more than one use_ok line in the file, this picks the last one.
By default presumes the current buffer shows a *.t file, but a T-FILE to examine
can be passed  in as an optional argument."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-package-name-from-useok"))
  (let* (
         (initial-buffer (current-buffer))
         (initial-point  (point))
         (useok "use_ok")
         ;; Note: this should skip any commented out 'use_ok' lines
         (find-pat (concat "^\\s*?" useok "\\b"))
         (capture-pat (perlnow-generate-capturing-pat))
         ;; (cmd-fmt "perl -e 'print %s'")
         useok-line   quoted-package   package-name  cmd
         )
    (save-excursion
      (if t-file
          (find-file t-file))
      (save-restriction
        (widen)
        (goto-char (point-max))
        (re-search-backward find-pat nil t)
        (setq useok-line (perlnow-extract-current-line))

        ;; trim string to the quoted package name with capture-pat
        (string-match capture-pat useok-line)

        ;; get the package-name with surrounding perl quotes
        (setq quoted-package
              (match-string 1 useok-line))

;;     ;; use perl to interpret the quoting
;;     ;; --or so I thought: *now* have issues with shell quoting the quotes...
;;         (setq cmd (format cmd-fmt (shell-quote-argument quoted-package)))
;;         (message "cmd: %s" (pp-to-string cmd))
;;         (setq package-name (shell-command-to-string cmd))

        (setq package-name
              (perlnow-strip-perl-quotage quoted-package))

        ;; covering for save-excursion flakiness
        (switch-to-buffer initial-buffer)
        (goto-char initial-point)
        ))
    package-name))

(defun perlnow-generate-capturing-pat ()
  "Generate a regexp to extract a package-name with quotes from a use_ok line.
This regexp punts on the problem of interpreting the myriad types
of allowed perl quotes and just includes them with the package-name."
  (let* (
         (capture-pat  ;; dodging need for a parser again (just barely)
             (concat
              "^[ \t]*?"
              "use_ok"

              "(?"      ;; allow a paren
              "[ \t]*?" ;; allow spaces
              "\\("     ;; open capture

              "\\(?:"   ;; non-capturing 'shy group'
              "['\"]"   ;;   simple double or single quotes...
              "\\|"     ;;   or...
              "q[^x]"   ;;   something like qq{ or q{, but *not* qx{
              "\\)"     ;; close shy group

              ".*?"     ;;   anything, including the quotes
              "\\)"     ;; close capture, right before either...

              "\\(?:"   ;; non-capturing 'shy group'
              ","       ;; a comma
              "\\|"     ;; or... something like a ");"
              "[ \t]*?" ;;  optional whitespace
              ")?"      ;;  optional closing paren
              "[ \t]*?" ;;  optional whitespace
              ";"       ;;  The end of all good things
              "\\)"     ;; close shy group
              ))
         )
    capture-pat))

;; special purpose hack of limited utility
(defun perlnow-strip-perl-quotage (str)
  "Remove perl-style quoting from the given STR and return it.
Perl quotes include various qq{} variants as well as singles and doubles."
;; Now, with extra-added ugly hackiness.
  (let* ((beg-pat-ng "^[ \t]*?\\(?:['\"]\\|q.?[\\W]?\\)[ \t]*?")
         (end-pat-ng "[ \t]*?['\"\\W]*[ \t]*?$")

         (pat1 "^[ \t'\"]")
         (pat2 "[ \t'\"]$")

         (pat3 "^qq?[{\(\[\|^#% ]+")
         (pat4 "[}\)\]\|^#% ]+$")

         (pat5 "[}|\)]$")
         )

    (setq str
          (replace-regexp-in-string pat1 "" str))
    (setq str
          (replace-regexp-in-string pat2 "" str))

    (setq str
          (replace-regexp-in-string pat3 "" str))
    (setq str
          (replace-regexp-in-string pat4 "" str))

    (setq str
          (replace-regexp-in-string pat5 "" str))

    (setq str
          (replace-regexp-in-string pat1 "" str))
    (setq str
          (replace-regexp-in-string pat2 "" str))
    str))



;; Example usage:
;;      (let* (
;;             (fields (perlnow-parse-standard-t-name test-file-full-path))
;;             (prefix      (nth 0 fields))
;;             (hyphenized  (nth 1 fields))
;;             (subname     (nth 2 fields))
;;             (description (nth 3 fields))
;;          )
(defun perlnow-parse-standard-t-name ( t-file )
  "Parses a perlnow standard test file name, T-FILE.
If everything is named according to typical perlnow conventions,
the test file name may look something like this:
   06-Funky-Wintermute-black_ice-verify_fatal.t

That will be subdivided into:
   06
   Funky-Wintermute
   black_ice
   verify_fatal

Where the four fields are: prefix, hyphenized, subname, description.
These fields are optional (though not all at the same time):
empty fields are returned as nil.

The fields are roughly delimited by hyphens, with the exception
of the transition between hyphenized module name and sub name,
which is marked by a case-change.

This naming convention is discussed in \\[perlnow-documentation-test-file-strategies].
"
  (let* (
         (t-file-basename
           (file-name-sans-extension (file-name-nondirectory t-file)))
         (fragments (split-string t-file-basename "-"))
         (integer-pat     "^[0-9]+$" )

         fragment words

         prefix hyphenized subname description
         )

    ;; get the leading numeric prefix, if any
    (if (string-match integer-pat (nth 0 fragments))
        (setq prefix (pop fragments)))

    ;; save up capitalized terms, bail when not capitalized
    (catch 'CHAIN
      (while (setq fragment (pop fragments))
        (cond ((perlnow-bigletter-p fragment)
               (push fragment words))
              (t
               (setq subname fragment)
               (throw 'CHAIN nil))
              )))

    ;; glue together words into hyphenized module name
    (setq hyphenized
          (mapconcat 'identity (reverse words) "-"))

    (if (string= hyphenized "")
        (setq hyphenized nil))

    ;; any remaining fragments are the description
    (if fragments
        (setq description
              (mapconcat 'identity fragments "-")))

    (list prefix hyphenized subname description)
    ))

;; Just used by perlnow-parse-standard-t-name
(defun perlnow-bigletter-p (str)
  "Try to determine whether the single-character string STR is a \"big letter\".
Which is to say, is it capitalized or title-case and not lowercase?
This just looks at the first character of STR, and silently ignores the rest."
  ;; This is a weird algorithm, but it beats trying to figure out what
  ;; get-char-code-property does.  (Maybe the regexp class [:upper:] would work?)
  (cond ((string= str "")
         "")
        (t
         (let*
             (
              (case-fold-search nil)
              (chr (substring str 0 1))
              (big-letter-pat "^[A-Z]"   )  ;; unicode, what's that?
              (small-letter-pat "^[a-z]" )
              (downcased   (downcase     chr))
              (upcased     (upcase       chr))
              (capitalized (capitalize   chr))
              ;; Note: for string= "Case is significant"
              (downcased-differs-p
               (string= chr downcased))
              (upcased-differs-p
               (string= chr upcased))
              (capitalized-differs-p
               (string= chr capitalized))
              bigletter-p )
           ;; The logic here:
           ;; (1) first check the ascii range for big and small letters.
           ;; (2) (a) if uc or ucfirst changes a character, it was small
           ;;     (b) if lc changes a character, then it was originally Big
           ;;     (c) if none change it, then it's neither big or small
           ;;         (punctuation, numeric, han...)
           (setq bigletter-p
                 (cond ((string-match big-letter-pat chr)
                        t)
                       ((string-match small-letter-pat chr)
                        nil)
                       ((and upcased-differs-p
                             capitalized-differs-p
                             (not downcased-differs-p))
                        t)
                       (t
                        nil) ))
           bigletter-p))))

(defun perlnow-incspot-from-t (testfile &optional md)
  "Given TESTFILE return the associated incspot.
An optional MD stash can be passed in provide hints. (TODO)."
  ;; TODO handle a t-dir argument as well as a testfile argument
  (if perlnow-debug
      (message "perlnow-incspot-from-t-plist: %s" (pp-to-string perlnow-incspot-from-t-plist)))
  (let* (
         ;; in lieu of save-excursion
         (initial-buffer (current-buffer))
         (initial-point  (point))

         (t-loc (perlnow-t-dir-from-t testfile))

         incspot
         staging
         )
    ;; make the testfile buffer open and active
    (find-file testfile)
    (cond ((setq staging (perlnow-find-cpan-style-staging-area))
           (setq incspot (concat staging perlnow-slash "lib"))
           )
;; TODO  (maybe someday...)
;;           (metadata
;;            (let* (
;;                   (testloc          (nth 0  metadata))
;;                   (dotdef           (nth 1  metadata))
;;                   (namestyle        (nth 2  metadata))
;;                   )
;;              (setq incspot
;;                    (perlnow-incspot-from-t-given-policy testfile testloc dotdef namestyle))
;;              ))
          (t
           (setq incspot
                 (perlnow-stash-lookup (file-name-directory testfile)))
           ))
    ;; return from any excursions
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    incspot))

;;          (setq testloc-absolute (file-name-directory testfile))
(defun perlnow-t-dir-from-t ( testfile )
  "Given a TESTFILE with absolute path, looks above it find a \"t\" directory.
Returns path to \"t\" (including \"t\")."
  (let* (
         ;; (path (perlnow-fixdir (file-name-directory testfile)))
         (path testfile) ;; starting here handles the no-intermediaries case better
         path-sans-slash dir t-loc
         )
    (setq t-loc
          (catch 'ROCK
            (while (> (length path) 1 ) ;; TODO unix-only?
              (setq path (perlnow-fixdir (concat path "..")))
              (setq path-sans-slash (replace-regexp-in-string "/$" "" path))
              (setq dir (file-name-nondirectory path-sans-slash))
              (if (string= dir "t")
                  (throw 'ROCK path))))
              )
    (if (< (length t-loc) 1)
        (setq t-loc nil))
    t-loc))

;; end of perlnow-module-from-t-file related functions


;;;--------
;;; more sub info: all subs in buffer
;;   (also see perlnow-sub-at-point above)

;; TODO currently unused:
;;    o  might be used to implement a side display, an overview and navigation aid. (better: ecb + etags?)
;;    o  might be used to talk to an existing tool like that (cedet?)
(defun perlnow-list-all-subs ( &optional internals )
  "Extracts the sub names for all routines in the current buffer.
Presumes the current buffer is a perl module.  If the INTERNALS
option is set to t, (TODO) subs with leading underscores are included,
otherwise they're skipped."
  ;; TODO implement internals
  (if perlnow-trace (perlnow-message "Calling perlnow-list-all-subs"))
  (interactive) ;; DEBUG only
  (if perlnow-trace (perlnow-message "Calling perlnow-list-all-subs"))
  (unless (perlnow-module-code-p)
    (error "perlnow-list-all-subs expects to be called from a module buffer."))
  (save-excursion
    ;; scrape through the module for sub names
    (goto-char (point-min))
    (let* ((sub-pattern (concat
                         "^[ \t]*"
                         "sub"
                         "[ \t]+"
                         "\\(.*?\\)"  ;; capture the sub-name
                         "[ \t]+"
                         ))
           (sub-list)
           )
      (while
          (re-search-forward sub-pattern nil t) ;; uses current buffer
        (progn
          (setq sub-name (match-string-no-properties 1))
          (if (or internals (not (string-match "^_" sub-name)))
                 (setq sub-list (cons sub-name sub-list)))
          ))
      sub-list
      )))

(defun perlnow-all-subs-report ()
  "Dump listing of all subs in current buffer."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-all-subs-report"))
  (let* (( sub-list (perlnow-list-all-subs))
         )
    (message "%s" sub-list)
    ))

;;=======
;; buffer navigation (( TODO rename section?  Move? ))

;; Used only by: perlnow-open-test-file
(defun perlnow-jump-to-use (package-name &optional import-string)
  "Given the PACKAGE-NAME, jumps to the point before the \'use\' line.
Specifically, these leaves the cursor at the start of the line
that does a \"use\" or \"use_ok\" of the named module specified in
perl's double-colon seperated form, e.g. \"Modular::Stuff\".
If given the optional IMPORT-STRING, incorporates it into the use line."
  (if perlnow-trace (perlnow-message "Calling perlnow-jump-to-use"))
  (if perlnow-debug
      (message "  perlnow-jump-to-use, package-name: %s import-string: %s"
               package-name
               (pp-to-string import-string)))
  (let (( pattern (format "^\\([ \t]*\\)use.*?\\b%s\\b" package-name))
        ( whitespace "" ))
    (goto-char (point-min))
    (re-search-forward pattern nil t)
    (setq whitespace (match-string 1))
    (cond (import-string
           (if (re-search-forward "'[ \t]*" nil t)
               (insert ", "))
           (insert import-string)
           ))
    (move-beginning-of-line 1)
    whitespace))

;;=======
;;  path crunching

(defun perlnow-full-path-to-module (incspot package-name)
  "Piece together a INC-SPOT and a PACKAGE-NAME into a full file name.
Given \"/home/doom/lib\" and the perl-style \"Text::Gibberish\" would
yield /home/doom/lib/Text/Gibberish.pm or in other words, the
filesys path."
  (if perlnow-trace (perlnow-message "Calling perlnow-full-path-to-module"))
  (let ((filename
         (concat
          (mapconcat 'identity (split-string package-name "::") perlnow-slash)
          ".pm")))
    (setq incspot (file-name-as-directory incspot))
    (concat  incspot filename)))

(defun perlnow-one-up (location)
  "Get an absolute path to the location one above the given LOCATION.
The empty string is treated as a synonym for root \(\"/\"\).
Relative locations are resolved by pre-pending the default-directory."
  (if perlnow-trace (perlnow-message "Calling perlnow-one-up"))
  (let* ( (slash perlnow-slash)
          (root slash)  ;; TODO anything more portable?
          )
    (setq location
          (cond ((string= location "")
                 root)
                (t
                 (perlnow-fixdir (concat location slash "..")))
                ))
    (if perlnow-trace
        (message "   Returning from 'oneup'"))
    location))

(defun perlnow-expand-dots-relative-to (dot_means given_path)
  "Using the dot definition DOT_MEANS, expand the GIVEN_PATH.
Given a directory path that leads with  \".\" or \"..\"
expand to an absolute path using the given DOT_MEANS as
the value for \".\".  Note: currently this is limited to
*leading* dot expressions, and can not handle weirder stuff
like: \"/home/doom/tmp/../bin\"."
  (if perlnow-trace
      (perlnow-message
       (concat "Calling perlnow-expand-dots-relative-to: "
               "dot_means: " (pp-to-string   dot_means) " "
               "given_path: " (pp-to-string   given_path) )))
  (let ((two-dot-pat "^\\.\\.")
        (one-dot-pat "^\\.")
           ;; must check two-dot-pat first or this could match there
        newpath  )
    (setq dot_means (perlnow-fixdir dot_means))
    (setq newpath
          (replace-regexp-in-string
           two-dot-pat
           (perlnow-one-up dot_means)
           given_path))
    (setq newpath
          (replace-regexp-in-string one-dot-pat dot_means newpath))
    (setq newpath (perlnow-fixdir newpath))
    (if perlnow-trace
        (message "   Returning from 'expand'"))
    newpath))

;;;==========================================================
;;; internal routines for perlnow-edit-test-file (and relatives)

;; TODO does this work from select menu?  ((I doubt it))
(defun perlnow-get-test-file-name ()
  "Looks for the test file for the current perl code buffer."
  (if perlnow-trace (perlnow-message "* Calling perlnow-get-test-file-name"))
  (let (testfile)
    (cond ( (perlnow-cpan-style-code-p)
            (setq testfile (perlnow-get-test-file-name-cpan-style)))
          ( (perlnow-module-code-p)
            (setq testfile (perlnow-get-test-file-name-module)))
          ( (perlnow-script-p)
            (setq testfile (perlnow-get-test-file-name-script)))
          (t
           (setq testfile (perlnow-get-test-file-name-script))))

    ;; TODO
    ;; (1) This is an okay place to set these, is it sufficient?
    ;; (2) Here, I'm saving the *last guess*.  More important: manual entries.
    (setq perlnow-recent-pick testfile)
    (setq perlnow-recent-pick-global testfile)  ;; TODO experimental

    testfile))

(defun perlnow-get-test-file-name-module ()
  "Get the test file name for the current perl module buffer.
Used by \\[perlnow-get-test-file-name]."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-test-file-name-module"))
  (perlnow-test-from-policy
     perlnow-test-policy-test-location-module
     perlnow-test-policy-dot-definition-module
     perlnow-test-policy-naming-style-module))

(defun perlnow-get-test-file-name-cpan-style ()
  "Get the test file name for the current perl module buffer.
  Used by \\[perlnow-get-test-file-name]."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-test-file-name-cpan-style"))
  (perlnow-test-from-policy
     perlnow-test-policy-test-location-cpan
     perlnow-test-policy-dot-definition-cpan
     perlnow-test-policy-naming-style-cpan
     ))

(defun perlnow-get-test-file-name-script ()
  "Get the test file name for the current perl script buffer.
Used by \\[perlnow-get-test-file-name]."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-test-file-name-script"))
  (perlnow-test-from-policy
     perlnow-test-policy-test-location
     perlnow-test-policy-dot-definition-script ;; "fileloc"
     perlnow-test-policy-naming-style-script   ;; "basename"
     ))

;;; Note: perlnow-edit-test-file docs explains a lot of what has to happen here.
(defun perlnow-test-from-policy (testloc dotdef namestyle)
  "Get the test file name for the current perl buffer, given a test policy.
This is used by \\[perlnow-get-test-file-name] and relatives.
A test policy \(see `perlnow-documentation-test-file-strategies'\)
is defined by three pieces of information:
the TESTLOC \(see `perlnow-test-policy-test-location'\)
the DOTDEF \(see `perlnow-test-policy-dot-definition' \)
and the NAMESTYLE \(see `perlnow-test-policy-naming-style'\)."
  (if perlnow-trace (perlnow-message "Calling perlnow-test-from-policy"))
  (if perlnow-debug
      (message "* perlnow-test-from-policy w/ %s %s %s" testloc dotdef namestyle ))
  (let* (
         (file-location
          (file-name-directory (buffer-file-name)))
         ;; script oriented info:
         (basename
          (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
         ;; intermediate info (calculated below):
         package-name   incspot   hyphenized-package-name
         staging-area
         ;; need to determine:
         testloc-absolute  test-file-from-policy  test-file
         )
    ;; module oriented info
    (cond
     (;; if module
      (setq package-name (perlnow-get-package-name-from-module-buffer))
      (setq incspot (perlnow-get-incspot package-name file-location))
      (setq hyphenized-package-name
            (mapconcat 'identity (split-string package-name "::") "-"))
      )
     (;; cpan-style fall back (scripts etc)
      (setq staging-area (perlnow-find-cpan-style-staging-area))
      (if perlnow-debug (message "staging-area: %s" staging-area))

      (setq incspot staging-area)
      (let* (
             ;; remove trailing slash so file-name-directory & nondirectory can work
             (last-slash-pat (concat perlnow-slash "$"))
             (incspot-trimmed
              (replace-regexp-in-string last-slash-pat "" incspot))
             (incspot-path      (file-name-directory    incspot-trimmed))
             (incspot-sans-path (file-name-nondirectory incspot-trimmed))
             )
        (if perlnow-debug (message "incspot-sans-path: %s" incspot-sans-path))
        (setq hyphenized-package-name incspot-sans-path)
        (setq package-name (mapconcat 'identity (split-string hyphenized-package-name "-") "::"))
        )))

    (if perlnow-debug (message "package-name: %s" package-name))

    (setq testloc-absolute
          (perlnow-testloc-from-policy testloc dotdef namestyle));;

    (if perlnow-debug (message "testloc-absolute: %s" testloc-absolute))

    ;; TODO break the following out as a test-file-from-policy? (or test-name?)
    (cond ((string= namestyle "hyphenized")  ;; only with modules
           (setq test-file-from-policy
                 (concat testloc-absolute hyphenized-package-name ".t")))

          ((string= namestyle "numeric")  ;; only with modules (TODO... but why not scripts, too?)
           (setq test-file-from-policy
                 (cond
                  ;; TODO try the -global form of this here
                  ((and perlnow-recent-pick
                        (file-exists-p perlnow-recent-pick))
                   perlnow-recent-pick )
                  ((perlnow-latest-test-file
                    (perlnow-most-recently-modified-file
                     (directory-files testloc-absolute t "\\.t$" nil))))
                  (t
                   (concat testloc-absolute
                           "01-" hyphenized-package-name ".t"))
                  )))
          ((string= namestyle "fullauto")  ;; originally just for modules, using for scripts now
           (setq test-file-from-policy
                 (cond ((perlnow-module-code-p)
                        (perlnow-fullauto-test-from-policy testloc-absolute hyphenized-package-name
                                                           "module"
                                                           basename ;; peer pressure
                                                           ))
                       ((perlnow-script-p)
                        (perlnow-fullauto-test-from-policy testloc-absolute hyphenized-package-name
                                                           "script"
                                                           basename ;; oh, don't ask why
                                                           ))
          ))
           )
          (;; just for scripts 'cause tests for 'em are stupid and so is basename
           (string= namestyle "basename")
           ;; TODO this isn't really fixed, instead I've been dancing around it with 'fullauto'
           (setq test-file-from-policy
                 (concat testloc-absolute basename ".t")))
          (t
           (error (concat "Invalid namestyle argument: "
                   namestyle ", must be hyphenized, basename or numeric."))
           ))
    (setq test-file test-file-from-policy)
    (if perlnow-debug (message "test-file: %s" test-file))
    test-file))


(defun perlnow-fullauto-test-from-policy (testloc-absolute hyphenized-package-name &optional mod-or-script basename)
  "Find a relevant test file to edit for the \"fullauto\" policy.
Begins by looking for existing tests in the test location:
TESTLOC-ABSOLUTE, tries to narrow the set by trying to find
matches on the module name (HYPHENIZED-PACKAGE-NAME)
and/or `perlnow-perl-sub-name'.
Ultimately chooses the most recently modified file
from the relevant set, or
if there are no tests found in TESTLOC-ABSOLUTE, we
return a file name to be created.
If MOD-OR-SCRIPT is set to 'script', operates on names with suffix \"-script.t\".
The option BASENAME exists because I'm abusing module-oriented code for the
script case (which is an abomination you should avoid using).
"
  (if perlnow-trace (perlnow-message "* Calling perlnow-fullauto-test-from-policy"))
  (if perlnow-debug
      (message
       (concat
        " fullauto-test, testloc-absolute: %s, "
        " hyphenized: %s, "
        " var perlnow-perl-sub-name: %s " )
       (pp-to-string testloc-absolute)
       (pp-to-string hyphenized-package-name)
       (pp-to-string perlnow-perl-sub-name)))
  (perlnow-sub-name-to-var)
  (cond ((not perlnow-perl-sub-name)
         (setq perlnow-perl-sub-name ""))) ;; nil causes problems
  (if perlnow-debug (message "   perlnow-perl-sub-name: %s" perlnow-perl-sub-name))

  (let* ( test-file-from-policy ;; return value
          test-name-pat
          test-files )
    (cond ((string= mod-or-script "script")
           (setq test-name-pat "-script\\.t$"))
          (t
           (setq test-name-pat "\\.t$")))
    (setq test-files (directory-files testloc-absolute t test-name-pat nil))
    (cond (test-files
           (let* ((test-files-module
                   (perlnow-grep-list test-files      hyphenized-package-name))
                  ;; unused:
                  ;; (test-files-subname  (perlnow-grep-list test-files      perlnow-perl-sub-name ))
                  (test-files-both
                   (perlnow-grep-list test-files-module perlnow-perl-sub-name ))
                  test-files-basename
                  )
             (cond (test-files-both ;; matches found on module *and* sub: pick latest
                    (setq test-file-from-policy
                          (perlnow-most-recently-modified-file test-files-both))
                    )
                   (t  ;; we're without match on module/sub, so...
                    (cond ((not perlnow-perl-sub-name) ;; no sub name defined
                           (cond ((test-files-module)   ;; have matches on module: will pick latest
                                  (setq test-file-from-policy
                                        (perlnow-most-recently-modified-file test-files-module))
                                  )
                                 ((string= mod-or-script "script")  ;; could be a funny place to do this TODO
                                  (cond (basename
                                         (setq test-files-basename
                                               (perlnow-grep-list test-files basename ))

                                         (setq test-file-from-policy
                                               (perlnow-most-recently-modified-file test-files-basename))
                                         )))
                                 (t ;; unused branch?  still no subname, plus not script and not modules-only matches
                                  )
                                 ))
                          (t ;; subname defined
                           (setq test-file-from-policy
                                 ;; just create a new one (gets sub name from var)
                                 (perlnow-new-test-file-name testloc-absolute hyphenized-package-name))
                           ))
                    ))))
          (t ;; no test files (*and* no subname) pick a new test name
            (setq test-file-from-policy
                  (perlnow-new-test-file-name testloc-absolute hyphenized-package-name))
            ))
    test-file-from-policy))


(defun perlnow-new-test-file-name ( testloc-absolute hyphenized-package-name)
  "Returns a new test file name to be created in TESTLOC-ABSOLUTE.
If there are test files in that location already, will use the maximum prefix
number plus 1, otherwise will use 01.
The name will include the HYPHENIZED-PACKAGE-NAME and, if
available, the `perlnow-perl-sub-name'.
If the current buffer is a script, will use an alternate naming style using
the script basename with the HYPHENIZED-PACKAGE-NAME, if defined."
  (if perlnow-trace (perlnow-message "Calling perlnow-new-test-file-name"))
  (let* ((prefix (perlnow-next-test-prefix testloc-absolute))
         new-name
         )
    (if perlnow-debug
        (message "testloc-absolute: %s  hyphenized-package-name: %s"
                 (pp-to-string testloc-absolute) (pp-to-string hyphenized-package-name)))
    (cond ((perlnow-module-code-p) ;; once again have a current-buffer dependency...
           (setq new-name
                 (cond (;; if we got a sub-name, use it
                        (and perlnow-perl-sub-name (not (string= perlnow-perl-sub-name "")))
                        (concat testloc-absolute
                                prefix "-" hyphenized-package-name "-" perlnow-perl-sub-name ".t"))
                       (t  ;; no sub-name, so skip it
                        (if perlnow-debug (message "perlnow-new-test-name: no sub-name"))
                        (concat testloc-absolute
                                prefix "-" hyphenized-package-name ".t")
                        ))) )
          ((perlnow-script-p)
           (let* ((basename
                   (file-name-sans-extension
                    (file-name-nondirectory (buffer-file-name))))
                  )
             (setq new-name
                   (cond ((and hyphenized-package-name (not (string= hyphenized-package-name "")))
                          ;; 02-Borgia-BananaPeel-deathskate-script.t"
                          (concat
                                testloc-absolute
                                prefix "-" hyphenized-package-name "-" basename "-script.t"))
                         (t
                          (concat
                                  testloc-absolute
                                  prefix "-" basename "-script.t")))
                   ))) )
    new-name))

(defun perlnow-next-test-prefix (testloc-absolute)
  "Look at the test files in TESTLOC-ABSOLUTE and get the next numeric prefix.
If no files are found in TESTLOC-ABSOLUTE, returns 01."
  (if perlnow-trace (perlnow-message "Calling perlnow-next-test-prefix"))
  (if perlnow-debug (message "   testloc-absolute: %s" (pp-to-string testloc-absolute)))

  (let* (test-files
         prefix-list
         numeric-list
         sorted-list
         next-numeric
         (next-prefix "01") ;; default
         )
    (cond (testloc-absolute
           (setq test-files (directory-files testloc-absolute t "\\.t$" nil))
           (cond (test-files
                  (setq prefix-list
                        (mapcar (lambda (file)
                                  (let* ((basename
                                          (file-name-sans-extension (file-name-nondirectory file))) )
                                    (car (split-string basename "-"))
                                    ))
                                test-files))
                  (setq numeric-list
                        (mapcar (lambda (item)
                                  (string-to-number item)
                                  )
                                prefix-list))
                  (setq sorted-list (sort numeric-list '<))
                  (setq next-numeric (1+ (car (last sorted-list)))) ;; TODO limit checking: what if over 99?
                  (setq next-prefix (format "%02d" next-numeric))
                  ))
           )
          (t
           (message "perlnow-next-test-prefix: working with a nil 'testloc-absolute' won't get far."))
          )
    next-prefix))

;; Used by perlnow-test-from-policy and hence perlnow-get-test-file-name,
(defun perlnow-testloc-from-policy (testloc dotdef namestyle)
  "Get the test file location for the current perl buffer, given a test policy.
This also tries to create the location if it doesn't already exist,
asking the user unless `perlnow-quiet' is set.
See \\[perlnow-test-from-policy] for the meaning of TESTLOC, DOTDEF and NAMESTYLE."
;; Actually, namestyle is unused here, but whatever.
  (if perlnow-trace
      (perlnow-message
       (concat
        "Calling perlnow-testloc-from-policy"
        " testloc: "    (pp-to-string testloc)
        " dotdef: "     (pp-to-string dotdef)
        " namestyle: "  (pp-to-string namestyle)
        )))
  (let* ( (file-name
           (cond ((buffer-file-name))
                 (perlnow-associated-code)
                 (t (error "perlnow-testloc-from-policy: Can't determine a perl code file."))
                 ))
          (file-location
            (file-name-directory file-name))

          testloc-absolute
          staging-area
          incspot
          package-name

         )
    (setq testloc-absolute
          (let (tla)
            (cond
             ;; cpan-style case (very simple)
             ((setq staging-area (perlnow-find-cpan-style-staging-area) )
              (setq tla (concat staging-area "t" perlnow-slash))
              )
             ;; if a module
             ((setq package-name
                    (perlnow-get-package-name-from-module-buffer))
              (setq incspot (perlnow-get-incspot package-name file-location))
              (setq tla
                    (cond ((string= dotdef "fileloc") ;; might be script or module
                           (perlnow-expand-dots-relative-to file-location testloc))
                          ((string= dotdef "incspot") ;; only with modules
                           (perlnow-expand-dots-relative-to incspot testloc))
                          (t
                           (error
                            (concat "Invalid perlnow-test-policy-dot-definition setting,"
                                    " should be 'fileloc' or 'incspot'")))))
              )
             ((perlnow-script-p)
              (setq tla
                    (cond ((string= dotdef "fileloc") ;; might be script or module
                           (perlnow-expand-dots-relative-to file-location testloc))
                          (t
                           (error
                            (concat "Invalid perlnow-test-policy-dot-definition setting,"
                                    " should be 'fileloc' or 'incspot'")))))
              ))
            tla))

    ;; ensure that testloc-absolute exists
    (perlnow-ensure-directory-exists testloc-absolute)
    testloc-absolute))

;; experimental -- currently not in use
(defun perlnow-ensure-test-file-exists (test-file)
  "If the given TEST-FILE doesn't exist, creates it using the test template.
The template used is specified by the variable `perlnow-perl-test-module-template'."
  (if perlnow-trace (perlnow-message "Calling perlnow-ensure-test-file-exists"))
  (let ((template perlnow-perl-test-module-template))
    (perlnow-ensure-file-exists test-file template)))

;;; TODO check if this is limited to cpan-style
(defun perlnow-list-test-files (testloc dotdef namestyle &optional fullpath-opt recurse-opt)
  "Looks for test files appropriate for the current file.
Uses the three given elements of a \"test policy\", to find
appropriate test files:
A test policy \(see `perlnow-documentation-test-file-strategies'\)
is defined by three pieces of information:
the TESTLOC \(see `perlnow-test-policy-test-location'\)
the DOTDEF \(see `perlnow-test-policy-dot-definition' \)
and the NAMESTYLE \(see `perlnow-test-policy-naming-style'\).
Note: actually NAMESTYLE isn't used internally: just a placeholder.
Returns file names with full path if FULLPATH-OPT is t.
If the RECURSE-OPT is set, lists files for the whole directory tree.
RECURSE-OPT implies FULLPATH-OPT.
" ;; TODO why not just remove the fullpath-opt?  I always want full paths.
  (if perlnow-trace (perlnow-message "Calling perlnow-list-test-files"))
  ;;;; Note, code mutated from above: perlnow-test-from-policy
  (if perlnow-debug
      (message "  perlnow-list-test-files: looking at buffer: %s" (buffer-name)))
  (let* ((full-file (buffer-file-name))
         (file-path
          (file-name-directory full-file))
         package-name   incspot   testloc-absolute
         test-file-list   )
    (cond (file-path
           ;; handle other cases?
           ;;    perlnow-find-cpan-style-staging-area
           ;;    perlnow-script-p
           ;;    perlnow-test-select-menu-p
           (cond (;; is module
                  (setq package-name (perlnow-get-package-name-from-module-buffer))
                  (setq incspot (perlnow-get-incspot package-name file-path))
                  )
                 ((perlnow-test-p)
                  (setq incspot (perlnow-incspot-from-t full-file))
                  )
                 ;;      (t ;; handle non-module, non-test cases,
                 ;;       )
                 )
           (if perlnow-debug
               (message "perlnow-list-test-files: file-path: %s testloc: %s " file-path testloc ))

           (setq testloc-absolute
                 (perlnow-fixdir
                  (cond ((string= dotdef "fileloc") ;; may be for script or module
                         (perlnow-expand-dots-relative-to file-path testloc))
                        ((string= dotdef "incspot") ;; only defined with modules
                         (cond (incspot
                                (perlnow-expand-dots-relative-to incspot testloc))
                               (t
                                (error (format "Could not determine incspot for file: %s" file-path))
                                ))
                         )
                        (t
                         (error (concat
                                 "Invalid perlnow-test-policy-dot-definition, "
                                 "should be fileloc or incspot"))))))

           (unless (file-directory-p testloc-absolute)
             (message "warning %s is not a directory" testloc-absolute))

           (cond ((file-directory-p testloc-absolute) ;; if loc is there, get the files there
                  (let* ((test-file-pat "\\\.t$"))
                    (setq test-file-list
                          (cond (recurse-opt
                                 (directory-files-recursively testloc-absolute test-file-pat)
                                 )
                                (t
                                 (directory-files testloc-absolute fullpath-opt test-file-pat)
                                 )))))
                 (t ;; if dir not there, we won't find anything will we?
                  (message "warning %s is not a directory" testloc-absolute)
                  (setq test-file-list ())
                  ))
           )
          (t ;; TODO but what about test select menu?  Or even, dired?
           (message "perlnow-list-test-files: buffer has no associated file, returning empty list.")
           (setq test-file-list ())
           ))
    (if perlnow-trace
        (message "   Returning from 'list-test-files'"))
    test-file-list))

;; Adding "harder" awareness to:  perlnow-edit-test-file
;;    C-u C-c \ t
;; TODO re-write to use perlnow-metadata
(defun perlnow-edit-test-file-harder (harder-setting)
  "Open a menu of all likely test files for user to choose from."
  (interactive
   (setq harder-setting (car current-prefix-arg)))
  (if perlnow-trace (perlnow-message "Calling perlnow-edit-test-file-harder"))
  (let ( testloc dotdef namestyle )
    (cond ((perlnow-cpan-style-code-p)
           (setq testloc   perlnow-test-policy-test-location-cpan  )
           (setq dotdef    perlnow-test-policy-dot-definition-cpan )
           (setq namestyle perlnow-test-policy-naming-style-cpan   )
            )
          ((perlnow-module-code-p)
           (setq testloc   perlnow-test-policy-test-location-module  )
           (setq dotdef    perlnow-test-policy-dot-definition-module )
           (setq namestyle perlnow-test-policy-naming-style-module   )
            )
          ((perlnow-script-p) ;; TODO try some time (ntigas)
           (setq testloc   perlnow-test-policy-test-location-script  )
           (setq dotdef    perlnow-test-policy-dot-definition-script )
           (setq namestyle perlnow-test-policy-naming-style-script   )
            )
          (t ;; other (whatever that would be...)
           (setq testloc   "../t" )
           (setq dotdef    "incspot" )
           ;; (setq namestyle "numbered") ;; nice name, too bad I never used it
           (setq namestyle "numeric")
           )
          )
    (perlnow-test-file-menu
     (perlnow-list-test-files testloc dotdef namestyle t
                              t ;; TODO EXPERIMENTAL recurse-opt
                              ))))


;;--------
;; test select menu

;; TODO
;;  o  Fix-up "Tests related to " heading.  Anything better?  Package name?
;;     Might not be all that closely related, either.  (( sounds like a job for metadata ))

(defun perlnow-test-file-menu (&optional test-file-list)
  "Show a buffer with a list of test files, allowing the user to choose one.
Defaults to looking up a list of tests using \\[perlnow-list-test-files],
but that may be over-ridden by passing in a different TEST-FILE-LIST."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-test-file-menu"))
  (let* ((md (perlnow-metadata)) ;; mainly for side-effect, but the info is useful, too
         (original-buffer (current-buffer))   ;; (nth 7 md) ?
         (original-file (buffer-file-name))   ;; (nth 8 md)
         ;; (location (file-name-directory     original-file))
         (filename (file-name-nondirectory  original-file))
         ;; (extension (perlnow-file-extension filename))
         (menu-buffer-name perlnow-select-test-file-buffer-name) ;; "*select test file*"
         (selection-buffer-label
          (format "Tests related to %s. To choose one, cursor to it and hit return."
                  filename))
         ;; uses policy to look up test location
         (testloc          (nth 0  md))
         (dotdef           (nth 1  md))
         (namestyle        (nth 2  md))

         t-menu-plist
         t-loc-list
         t-loc
         )
    (unless test-file-list
      (setq test-file-list
            (perlnow-list-test-files
             testloc dotdef namestyle
             t ;; full-path
             t ;; recursive
             )))
    (perlnow-show-buffer-other-window menu-buffer-name)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))

    (put-text-property
       0 (length selection-buffer-label) 'face 'perlnow-00-face selection-buffer-label)
    (insert selection-buffer-label)
    (insert "\n")

    ;; Convert to lists of files in each directory, gathered in one plist, keyed by directory
    (dolist (fullfile test-file-list)
      (let (loc file loclist)
        (setq loc  (file-name-directory    fullfile))
        (setq file (file-name-nondirectory fullfile))
;;        (setq file (perlnow-markup-file-with-path fullfile)) ;; file marked-up with path propety
        (setq loclist (or
                       (perlnow-stash-lookup loc 't-menu-plist)
                       () ))
;;        (push file loclist)
        (push fullfile loclist)
        (setq loclist (sort loclist 'string<))
        (perlnow-stash-put loc loclist 't-menu-plist)
      ))
    (if perlnow-debug
        (message "t-menu-plist: %s" (pp-to-string t-menu-plist)))

    ;; sorted list of the keys, the test locations
    (setq t-loc-list
          (sort
           (perlnow-plist-keys t-menu-plist)
           'string< ))
    (if perlnow-debug
        (message "t-loc-list: %s" (pp-to-string t-loc-list)))

    ;; to support multi-location sets, we need to loop over t-locs
    (dolist (loc t-loc-list)
      (let ( loc-str file file-list )
        (setq filelist (perlnow-stash-lookup loc 't-menu-plist))

        (setq loc-str (concat loc ": "))
        (put-text-property
           0 (length loc-str) 'face 'perlnow-01-face loc-str)
        (insert loc-str)
        (insert "\n")

        (insert "   ")
        (dolist (str (mapcar 'perlnow-markup-file-with-path filelist))
          (put-text-property 0 (length str) 'face 'perlnow-02-face str)
          (insert str)
          (insert "\n   ")
          )
        (insert "\n")
      ))

    (perlnow-select-mode)
    (setq perlnow-associated-code original-file) ;; connect menu back to generating context
    (setq perlnow-associated-buffer original-buffer) ;; Experimental TODO
    (setq buffer-read-only 't)
    (goto-char (point-min))

    (if perlnow-debug
     (message "MENU: perlnow-recent-pick: %s" perlnow-recent-pick))
    (if perlnow-debug
        (message "MENU: perlnow-recent-pick-global: %s" perlnow-recent-pick-global))

    ;; position cursor over the last file that was selected
    (cond ( perlnow-recent-pick-global
            (let* (
                   (lastname (file-name-nondirectory perlnow-recent-pick-global))
                   (lastpath (file-name-directory    perlnow-recent-pick-global))
                   )
              (cond ( (and
                       (search-forward (concat lastpath ": ") nil t)
                       (search-forward (concat lastname) nil t) )
                      ))))
          (t
           (next-line 2)
           ))
    ;; move to beginning of line, then beginning of next file name
    (move-beginning-of-line 1)
    (goto-char (next-single-property-change (point) 'perlnow-file-path))
    ;; just to return something.
    menu-buffer-name))


;; lifted from my old rep.el code:
(defmacro perlnow-make-face (name number color1 color2)
  "Generate a colorized face suitable to markup changes.
NAME is the name of the face, COLOR1 is for light backgrounds
and COLOR2 is for dark backgrounds.
NUMBER is the corresponding rep substitution number (used only
in the doc string for the face."
  `(defface ,name
  '((((class color)
      (background light))
     (:foreground ,color1))
    (((class color)
      (background dark))
     (:foreground ,color2)))
  ,(format "Face used for changes from substitution number: %s." number)
  :group 'desktop-recover-faces
  ))

(perlnow-make-face perlnow-00-face 00 "DarkGoldenrod4" "DarkGoldenrod2")
(perlnow-make-face perlnow-01-face 01 "MediumPurple4" "MediumPurple1")
(perlnow-make-face perlnow-02-face 02 "forest green" "light green")
(perlnow-make-face perlnow-03-face 03 "PaleVioletRed4" "PaleVioletRed1") ;; hot pink
(perlnow-make-face perlnow-04-face 04 "gold4" "gold1")
(perlnow-make-face perlnow-05-face 05 "salmon4" "salmon1")
(perlnow-make-face perlnow-06-face 06 "RoyalBlue1" "RoyalBlue1")
(perlnow-make-face perlnow-07-face 07 "DarkOrchid4" "DarkOrchid1")
(perlnow-make-face perlnow-08-face 08 "green4" "green1")
(perlnow-make-face perlnow-09-face 09 "khaki1" "khaki4")
(perlnow-make-face perlnow-10-face 10 "DarkOrange4" "DarkOrange1")
(perlnow-make-face perlnow-11-face 11 "SeaGreen4" "SeaGreen1")
(perlnow-make-face perlnow-12-face 12 "maroon4" "maroon1")
(perlnow-make-face perlnow-13-face 13 "firebrick4" "firebrick1")
(perlnow-make-face perlnow-14-face 14 "PeachPuff4" "PeachPuff1")
(perlnow-make-face perlnow-15-face 15 "CadetBlue4" "CadetBlue1")
(perlnow-make-face perlnow-16-face 16 "aquamarine4" "aquamarine1")
(perlnow-make-face perlnow-17-face 17 "OliveDrab4" "OliveDrab1")
(perlnow-make-face perlnow-18-face 18 "SpringGreen4" "SpringGreen1")
(perlnow-make-face perlnow-19-face 19 "chocolate4" "chocolate1")
(perlnow-make-face perlnow-20-face 20 "DarkSeaGreen4" "DarkSeaGreen1")
(perlnow-make-face perlnow-21-face 21 "LightSalmon4" "LightSalmon1")
(perlnow-make-face perlnow-22-face 22 "DeepSkyBlue4" "DeepSkyBlue1")
(perlnow-make-face perlnow-23-face 23 "chartreuse4" "chartreuse1")
(perlnow-make-face perlnow-24-face 24 "cyan4" "cyan1")
(perlnow-make-face perlnow-25-face 25 "magenta4" "magenta1")
(perlnow-make-face perlnow-26-face 26 "blue4" "blue1")
(perlnow-make-face perlnow-27-face 27 "DeepPink4" "DeepPink1")
(perlnow-make-face perlnow-28-face 28 "DarkOliveGreen4" "DarkOliveGreen1")
(perlnow-make-face perlnow-29-face 29 "coral4" "coral1")
(perlnow-make-face perlnow-30-face 30 "PaleGreen4" "PaleGreen1")
(perlnow-make-face perlnow-31-face 31 "tan4" "tan1")
(perlnow-make-face perlnow-32-face 32 "orange4" "orange1")
(perlnow-make-face perlnow-33-face 33 "cornsilk4" "cornsilk1")

(perlnow-make-face perlnow-34-face 34 "SlateGray4" "SlateGray1")
(perlnow-make-face perlnow-35-face 35 "LightSkyBlue4" "LightSkyBlue1")
(perlnow-make-face perlnow-36-face 36 "SkyBlue4" "SkyBlue1")
(perlnow-make-face perlnow-37-face 37 "SteelBlue4" "SteelBlue1")
(perlnow-make-face perlnow-38-face 38 "DimGray" "LightGray")

(defun perlnow-markup-file-with-path (fullfile)
    "Converts file name with path to file name with path as text-property.
This sets the text property perlnow-file-path for every character in the name."
  (if perlnow-trace (perlnow-message "Calling perlnow-markup-file-with-path"))
    (let* (
           (path (file-name-directory    fullfile))
           (name (file-name-nondirectory fullfile))
           (start 0)
           (end (length name))
           )
      (put-text-property start end 'perlnow-file-path path name)
      name))

(defun perlnow-get-path-from-markedup-name (name)
  "Returns the value of the property perlnow-file-path for string NAME.
This only checks the first character in NAME."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-path-from-markedup-name"))
  (let* ((path
          (get-text-property 0 'perlnow-file-path name))
         )
    path))

;; DEBUG trial runs:
;; (perlnow-markup-file-with-path "/home/doom/tmp/WTF/UberAlles.pm")
;; (perlnow-get-path-from-markedup-name (perlnow-markup-file-with-path "/home/doom/tmp/WTF/UberAlles.pm"))

(define-derived-mode perlnow-select-mode
  text-mode "perlnow-select"
  "Major mode to display items from which user can make a selection.
\\{perlnow-select-mode-map}"
  (use-local-map perlnow-select-mode-map))

;; TODO put this in a keybinding setup routine,
;;      and call it from a perlnow-setup (not yet created).
(define-key perlnow-select-mode-map "\C-m"     'perlnow-select-file)
(define-key perlnow-select-mode-map "n"        'next-line)
(define-key perlnow-select-mode-map "p"        'previous-line)
(define-key perlnow-select-mode-map [tab]      'perlnow-select-forward-hotspot)
(define-key perlnow-select-mode-map [backtab]  'perlnow-select-previous-hotspot)
(define-key perlnow-select-mode-map "a"        'perlnow-select-create-test)
(define-key perlnow-select-mode-map "\C-c/b"   'perlnow-back-to-code)       ;; TODO experimental binding
                                                                            ;;  (disconcerting it's one-way)

;; These seem kind of hacky, but I think that's the nature of next-single-property-change
(defun perlnow-select-forward-hotspot ()
  "Skip forward to the next file selection \"hotspot\"."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-select-forward-hotspot"))
  (let* ((next-loc (next-single-property-change (point) 'perlnow-file-path)))
    (cond (next-loc
           (goto-char next-loc))))
  (unless (get-text-property (point) 'perlnow-file-path)
    (let* ((next-loc (next-single-property-change (point) 'perlnow-file-path)))
      (cond (next-loc
             (goto-char next-loc)))))
  (move-beginning-of-line 1)
  (goto-char (next-single-property-change (point) 'perlnow-file-path)))

(defun perlnow-select-previous-hotspot ()
  "Skip forward to the next file selection \"hotspot\"."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-select-previous-hotspot"))
  (let* ((prev-loc (previous-single-property-change (point) 'perlnow-file-path)))
    (cond (prev-loc
           (goto-char prev-loc))))
  (unless (get-text-property (1- (point)) 'perlnow-file-path)
    (let* ((prev-loc (previous-single-property-change (point) 'perlnow-file-path)))
      (cond (prev-loc
              (goto-char prev-loc)))))
  (move-beginning-of-line 1)
  (goto-char (next-single-property-change (point) 'perlnow-file-path)))

(defun perlnow-select-file ()
  "Choose the item on the current line."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-select-file"))
  (let ( initiating-buffer
         selected-file-compact
         selected-file
         path
         original-context
         newly-selected-buffer
         newly-selected-file-name
         )
    ;; The code buffer the menu was generated from
    (setq initiating-buffer perlnow-associated-code)

    (setq selected-file (perlnow-select-read-full-file-name))

    ;; trace associated pointers back to code being tested
    (setq original-context
          (perlnow-follow-associations-to-non-test-code initiating-buffer))
    (if perlnow-debug
        (message
         (concat
          (format "%30s %-40s\n" "selected-file: "     selected-file)
          (format "%30s %-40s\n" "initiating-buffer: " initiating-buffer)
          (format "%30s %-40s\n" "original-context: "  original-context)
          )))

    ;; open the selection, then set-up pointer back to original code
    (find-file selected-file)
    (setq newly-selected-buffer    (current-buffer))
    (setq newly-selected-file-name (buffer-file-name))
    (setq perlnow-associated-code  original-context)

    ;; switch to the original, point at the newly opened test file
    (set-buffer (find-buffer-visiting original-context))
    (setq perlnow-associated-code newly-selected-file-name)
    (setq perlnow-recent-pick     newly-selected-file-name)
    (setq perlnow-recent-pick-global  newly-selected-file-name)  ;; TODO experimental
    (if perlnow-debug
        (message "SELECT: perlnow-recent-pick-global: %s" perlnow-recent-pick-global))

    ;; make the newly opened buffer active, display original code in parallel
    (switch-to-buffer newly-selected-buffer)
    (perlnow-show-buffer-other-window (find-buffer-visiting original-context) nil t)
    ;; just to return something, apparently
    selected-file))

;; Used by perlnow-select-file
(defun perlnow-select-file-from-current-line ()
  "This version just returns the current line as a string."
  (if perlnow-trace (perlnow-message "Calling perlnow-select-file-from-current-line"))
  (save-excursion
    (let (selected-file beg end)
      ;; TODO refactor to use: (setq selected-file (perlnow-extract-current-line))
      ;; TODO REFACTOR BEG
      (move-beginning-of-line 1)
      (setq beg (point))
      (move-end-of-line 1)
      (setq end (point))
      (setq selected-file (buffer-substring beg end))
      ;; TODO REFACTOR END
      ;; trim leading/trailing spaces

      ;; TODO refactor to use:
      ;;   (setq selected-file (perlnow-strip-leading-trailing-spaces selected-file))
      ;; TODO REFACTOR BEG
      (setq selected-file
            (replace-regexp-in-string "^\s+" "" selected-file))
      (setq selected-file
            (replace-regexp-in-string "\s+$" "" selected-file))
      ;; TODO REFACTOR END
      selected-file)))

;; I seem to want to do this all the time...
;; general buffer string utility
(defun perlnow-extract-current-line ()
  "Get the current line from the buffer and return it as a string."
  (save-excursion  ;; this might even work, without a find-file in there.
    (save-restriction
      (widen)
      (let (current-line)
        (move-beginning-of-line 1)
        (setq beg (point))
        (move-end-of-line 1)
        (setq end (point))
        (setq current-line (buffer-substring beg end))
        current-line))))

;; general string utility
(defun perlnow-strip-leading-trailing-spaces (str)
  "Remove leading and trailing spaces from the given STR and return it."
  ;; trim leading/trailing spaces
  (setq str
        (replace-regexp-in-string "^\s+" "" str))
  (setq str
        (replace-regexp-in-string "\s+$" "" str))
  str)


;; Used indirectly by perlnow-select-file
(defun perlnow-select-read-full-file-name ()
  "In select test buffer, tries to read a file name with path from current line.
Returns nil if not inside a *perlnow test select* buffer, or if
no file-name is found on the current line."
  (let (selected-file-compact path selected-file)
    (cond ((perlnow-test-select-menu-p)
           (setq selected-file-compact (perlnow-select-file-from-current-line))
           (cond ((or selected-file-compact (not (string= selected-file-compact "")))
                  (setq path (perlnow-get-path-from-markedup-name selected-file-compact))
                  (setq selected-file (concat path selected-file-compact))))
           ))
    selected-file))

;; bound to "a" in perlnow-select-mode
;; Note: somewhat similar to perlnow-test-create-manually
(defun perlnow-select-create-test (&optional testfile )
  "Create a new test file via minibuffer with a default
that assumes the test on the current line is for the module
you're interested in (and is named using the module name
according to \"perlnow standard\").  The newly generated
test will not be named with any sub name \(though you can
add one later\).
The default TESTFILE can also be supplied as a function argument."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-select-create-test"))
  (let (file path hyphenized next-prefix)
    (cond ((not testfile)
           (setq file (perlnow-select-file-from-current-line))
           (setq path (perlnow-get-path-from-markedup-name file))

           ;; module name from current line, hyphenized
           ;; Literally what I want, using abstract wrapper instead:
           ;;   perlnow-extract-hyphenized-from-standard-t-name
           (setq hyphenized (perlnow-module-from-t-file file))
           (setq next-prefix (perlnow-next-test-prefix path))

           (setq testfile (concat path next-prefix "-" hyphenized ".t"))
           )))

  (let* ((tf-input
          (read-from-minibuffer
           "Test file: "
           testfile
           nil
           nil
           (cons 'perlnow-test-file-history 2) ;; TODO double-check the 2
           ))
         )
    (setq perlnow-recent-pick        tf-input)
    (setq perlnow-recent-pick-global tf-input)
    (perlnow-open-test-file          tf-input)
    ))

;; end of test file selection menu code
;;--------
;; inc display buffer (reuses some components of test select menu)
;;

(define-derived-mode perlnow-inc-mode
  perlnow-select-mode
  "perlnow-inc"
  "Major mode to display perl'S @INC array.
\\{perlnow-inc-mode-map}"
  (use-local-map perlnow-inc-mode-map))

;; ;; TODO put keybindings in a setup routine,
;; ;;      and call it from a perlnow-setup (not yet created).

;; TODO new command to refresh the display, bind to "g"

;; TODO clone module/cpan create commands, have them use (and revise)
;; dev/lib/bin settings based on current selection

(define-key perlnow-inc-mode-map "g"        'perlnow-inc-refresh)
(define-key perlnow-inc-mode-map "a"        'perlnow-inc-add-inc)
(define-key perlnow-inc-mode-map "d"        'perlnow-inc-remove-inc)
(define-key perlnow-inc-mode-map "\C-m"     'perlnow-inc-dired)

;; same as perlnow-select-mode bindings:
(define-key perlnow-inc-mode-map "n"        'next-line)
(define-key perlnow-inc-mode-map "p"        'previous-line)
(define-key perlnow-inc-mode-map [tab]      'perlnow-select-forward-hotspot)
(define-key perlnow-inc-mode-map [backtab]  'perlnow-select-previous-hotspot)
(define-key perlnow-inc-mode-map "\C-c/b"   'perlnow-back-to-code)

;; TODO
;;  o  make the listed names hot-- e.g. hit return, go into dired  (( DONE: kiss for now ))
;;  o  Or: list all modules *and* versions in incspot
;;  o  Further: integrate with CPAN.pm/CPANPLUS/cpanm-- upgrade command
(defun perlnow-display-inc-array (&optional this-window)
  "Show a listing of all locations in perl's @INC array.
Opens a buffer called \"perlnow @INC\" in  other window.
Returns the name of display buffer.
If THIS-WINDOW is t, just display in the current window."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-display-inc-array"))
  (let* ((inc-path-list (perlnow-all-incspots))
         (display-buffer "*perlnow @INC*")
         (buffer-label
          (format " @INC locations for perl:"))
         (switch-back   nil)

         ;; (md (perlnow-metadata))
         (original-buffer (current-buffer))   ;; (nth 7 md)
         (original-file (buffer-file-name))   ;; (nth 8 md)
         ;; (location (file-name-directory     original-file))
         ;; (filename (file-name-nondirectory  original-file))
         ;; (extension (perlnow-file-extension filename))
         )

    ;; color for the buffer-label
    (put-text-property 0 (length buffer-label)
                       'face 'perlnow-00-face
                       buffer-label)
    (perlnow-blank-out-display-buffer display-buffer switch-back)
    (cond (this-window
           (switch-to-buffer display-buffer)
           )
          (t ;; default, display in other window
           (perlnow-show-buffer-other-window display-buffer)
           ))

    (let ((buffer-read-only nil)) ;; make buffer writeable, temporarily
      (insert buffer-label)
      (insert "\n")
      (dolist (path inc-path-list)
        (let ( path-str line )
          (setq path-str path)
          (cond ((file-writable-p path)
                 ;; color blue, to indicate you can create stuff there
                 (put-text-property 0 (length path-str)
                                    'face 'perlnow-37-face
                                    path-str)
                 )
                (t
                 ;; no write-access, color gray (34 or 38)
                 (put-text-property 0 (length path-str)
                                    'face 'perlnow-34-face
                                    path-str)
                 ))
          ;; hack: makes this a "hotspot"
          (put-text-property 0 (length path)
                             'perlnow-file-path path
                             path-str)
          (setq line (format "   %s\n" path-str))
          (insert line)
          ))
      (deactivate-mark t)
      )
    ;; (perlnow-select-mode)
    (perlnow-inc-mode)
    (setq perlnow-associated-code original-file) ;; connect menu back to generating context
    (setq perlnow-associated-buffer original-buffer)

    ;; make buffer read-only now
    (setq buffer-read-only t)
    ;; park cursor at start of first entry
    (goto-char (point-min))
    (forward-line 1)
    (forward-word 1)
    (forward-word -1)
    (backward-char 1)
    display-buffer))

;; bind to "return"
(defun perlnow-inc-dired ()
  "Get path off current line, open in dired in other window."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-inc-dired"))
  (let ( initiating-buffer
         selected-dir
         )
    ;; The code buffer the menu was generated from
    (setq initiating-buffer perlnow-associated-code)
    (setq selected-dir (perlnow-select-file-from-current-line))

    (message "selected-dir: %s" selected-dir)
    (other-window 1) ;; no-op if only one window in frame
    (dired selected-dir)
    ;; just to return something, apparently
    selected-dir))

(defun perlnow-inc-add-inc (incspot)
  "Add a new location to perl's @INC."
  (interactive "GNew location for @INC: ")
  (if perlnow-trace (perlnow-message "Calling perlnow-inc-add-inc"))
  (let* ((selected-dir (perlnow-select-file-from-current-line))
         (clean-inc  (expand-file-name incspot))
         (clean-spot (substring-no-properties selected-dir))
         )
    (perlnow-add-incspot-to-perl5lib clean-inc clean-spot)
    (perlnow-inc-refresh)
    ))

(defun perlnow-inc-remove-inc ()
  "Remove current location from perl's @INC."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-inc-remove-inc"))
  (let ((selected-dir (perlnow-select-file-from-current-line)))
    (cond (selected-dir
           (perlnow-remove-incspot-from-perl5lib
            ;; (expand-file-name
             selected-dir
            ;; )
            )
           (perlnow-inc-refresh)
           )
          (t
           (message "First, position cursor on the entry you want to remove.")
           ))
    ))


(defun perlnow-inc-refresh ()
  "Refresh the display of the @INC buffer."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-inc-refresh"))
  (let ( initiating-buffer
         initial-point
         )
    (setq initial-point (point))
    ;;     ;; the buffer we were in when we ran this command...
    ;;     (setq initiating-buffer perlnow-associated-code)
    ;; (other-window 1) ;; cheesy?
    (perlnow-display-inc-array t)
    ;; (switch-to-buffer initiating-buffer) ;; probably not needed.
    (goto-char initial-point)
  ))

;; end of inc display code
;;
;;--------


;; Compatibility wrapper, used to swap in new behavior
(defun perlnow-latest-test-file (test-file-list)
  "Given a list of test files, select the \"latest\" one.
By latest, we mean the one that's most recently modified,
on the theory that that's the one you're likely to want to
work on again."
  (if perlnow-trace (perlnow-message "Calling perlnow-latest-test-file"))
  (let ((latest
         (perlnow-most-recently-modified-file test-file-list)) )
    (if perlnow-trace
        (message "   Returning from 'latest-test-file'"))
    latest))

;;--------
;; getting the most recently modified file

(defun perlnow-file-mtime (filename)
  "Return the mtime for the given FILENAME."
  (if perlnow-trace (perlnow-message "Calling perlnow-file-mtime"))
  (let* (( attribs    (file-attributes filename) )
         ( mtime-pair (nth 5 attribs) )
         ( mtime-high (nth 0 mtime-pair))
         ( mtime-low  (nth 1 mtime-pair))
         ( mtime      (+ (* 65536 mtime-high) mtime-low))
         )
    mtime))

(defun perlnow-file-mtime-p (a b)
  "A \"predicate\" to sort files in order of decreasing age."
  (if perlnow-trace (perlnow-message "Calling perlnow-file-mtime-p"))
  (> (perlnow-file-mtime a) (perlnow-file-mtime b)))

(defun perlnow-sort-file-list-by-mtime (list)
  "Given the LIST of file names, sorts it by mtime."
  (if perlnow-trace (perlnow-message "Calling perlnow-sort-file-list-by-mtime"))
  (let* ((sorted-list (sort list 'perlnow-file-mtime-p)) )
    (if perlnow-trace
        (message "   Returning from 'sort-file-list-by-mtime'"))
    sorted-list))

(defun perlnow-most-recently-modified-file (list)
  "Get the most recently modified file, given a LIST of files."
  (if perlnow-trace (perlnow-message "Calling perlnow-most-recently-modified-file"))
  (let ((most-recent
         (car (perlnow-sort-file-list-by-mtime list))))
    (if perlnow-trace
        (message "   Returning from 'most-recently-modified-file'"))
    most-recent))

;; An older approach (circa 0.4), not currently in use
(defun perlnow-latest-test-file-numeric-prefix (test-file-list)
  "Given a list of test files, select the \"latest\" one.
By latest, we mean the one a developer is most likely to want to
work on, which currently means the the one with the largest
numeric sort-order prefix."
  (if perlnow-trace (perlnow-message "Calling perlnow-latest-test-file-numeric-prefix"))
  (let* ((new-list
           ;;            (perlnow-grep "^[0-9]*?-" test-file-list))
           ;; (perlnow-grep "/[0-9]*?-.*?$" test-file-list))
           (perlnow-grep-list test-file-list "/[0-9]*?-.*?$"))
         (new-list (sort new-list 'string<))
         (last-item (car (last new-list)))
         )
    last-item))

;;--------
;;

(defun perlnow-lookup-preferred-perl-mode ()
  "Look-up which perl mode the user prefers.
Examines the alists `interpreter-mode-alist' and
`auto-mode-alist' to see if perl-mode,
cperl-mode \(or perhaps something else entirely?\)
has been chosen as the default to work on perl code."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-lookup-preferred-perl-mode"))
  (let* ( (default "cperl-mode")
          (mode default)
          (interpreter-rule "perl") ;; should match perl or perl5 ;; TODO risk of perlnow-*-mode
          (auto-rule "\[[pP][pP]\]\[[Llm][Llm][Llm]\]")
               ;; regexp to match a regexp containing: [pP][Llm]
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


(defun perlnow-assoc-regexp (pattern alist &optional default)
  "Return first value from ALIST with key that matches PATTERN."
  (if perlnow-trace (perlnow-message "Calling perlnow-assoc-regexp"))
  (assoc-default pattern alist 'string-match default))



;; concept: if given a module, you can get the module root
;; and from there, you peek up one level (check for "../t"),
;; and scan downwards, adding the "t"s found to the list.
;; Order should be top to bottom.;; (and how to deal with scripts?
;; for now: use script location in place of module root.
;; exception: if it's inside a cpan-style package).
(defun perlnow-find-t-directories ()
  "Find 't' directories associated with current file.
Order them in rough order of likely priority. At present, that is
just \"top to bottom\", currently this does not use the test
policy settings.  Note: this code looks for \"t\" files directly
adjacent to one of the significant levels of the code's path,
it does not, for example, do a full recursive descent from
a module's incspot."
  (if perlnow-trace (perlnow-message "Calling perlnow-find-t-directories"))
  (let* ( (slash (convert-standard-filename "/"))
          (levels () )
          (t-list () )
          )
    (cond ((perlnow-module-code-p)
            (let* (
                   (pm-file (buffer-file-name))
                   (pm-location (file-name-directory pm-file))
                   (package-name (perlnow-get-package-name-from-module-buffer))
                   (incspot (perlnow-get-incspot package-name pm-location))
                   (candidate)
                   (tail)
                   )
              (setq candidate (perlnow-fixdir (concat incspot "../t")))
              (if (file-directory-p candidate)
                  (setq t-list (cons candidate t-list)))
              ;;; subtract incspot from pm-location...
              (if (string-match (concat "^" incspot "\\\(.*?\\\)$") pm-location)
                  (setq tail (match-string 1 pm-location)))
              ;;; split up the tail, ala what's done in perlnow-nth-file-path-level
              (setq levels (split-string tail slash t)) ;; omit-nulls is on
              ;; append levels one at a time to incspot,
              ;; looking for "t" dirs at each level
              (let ( (path incspot)
                     )
                (dolist (level levels)
                  (setq path (concat path level slash))
                  (setq candidate (perlnow-fixdir (concat path "t")))
                  (if (file-directory-p candidate)
                      (setq t-list (cons candidate t-list)))
                  ))
              ))
          (t ;; assume it's a script then
           (let* ( (full-file (buffer-file-name))
                   (location (file-name-directory full-file))
                   (filename (file-name-nondirectory full-file))
                   )

             (setq candidate (perlnow-fixdir (concat location "../t")))
             (if (file-directory-p candidate)
                 (setq t-list (cons candidate t-list)))

             (setq candidate (perlnow-fixdir (concat location "t")))
             (if (file-directory-p candidate)
                 (setq t-list (cons candidate t-list)))

             ))
          )
    t-list
    ))


;; TODO can this idea be blended with the job that
;; perlnow-find-t-directories does (finding the most appropriate
;; t-dir).  How do you prioritize these?
;; (( This isn't really in use. ))
(defun perlnow-find-all-t-directories (&optional root)
  "Find all directories named \"t\" in the tree.
Looks under the given ROOT, or under the `default-directory'.
Note: at present, this has nothing to do with \\[perlnow-find-t-directories]."
  (if perlnow-trace (perlnow-message "Calling perlnow-find-all-t-directories"))
  (unless root
    (setq root default-directory))
  (perlnow-recursive-file-listing root "/t$" "d"))

;; (perlnow-find-all-t-directories "/home/doom/End/Cave/EmacsPerl/Wall/")


;;; The end of perlnow-edit-test-file family of functions


;;;==========================================================
;;;  file-system probes (etc.)

(defun perlnow-how-to-perl ()
  "Define how to run perl for the current buffer.
Gives precedence to the way it's done with the hash-bang line if
that's found at the top of the file \(this preserves whatever
path and options the author of the code intended, e.g. the \"-T\"
flag\).  If that's not found, it uses the contents of the
`perlnow-perl-program' variable, and if that has not been defined
falls back to just \"perl\"."
  (if perlnow-trace (perlnow-message "Calling perlnow-how-to-perl"))
  (let* ((perl-from-hashbang (perlnow-hashbang))
         (how-to-perl ""))
    (cond (perl-from-hashbang
            (setq how-to-perl perl-from-hashbang))
          (perlnow-perl-program
            (setq how-to-perl perlnow-perl-program))
          (t
           (setq how-to-perl "perl")))
    how-to-perl))

(defun perlnow-find-cpan-style-staging-area ( &optional file-name )
  "Determines if the current file buffer is located in an cpan-style tree.
Should return the path to the current cpan-style staging area, or
nil if it's not found.  The staging area is located by searching
upwards from the location of a file to a location with files that
look like a cpan-style project (as currently implemented, it
looks for either a \"Makefile.PL\" or a \"Build.PL\"\).
This defaults to working on the current buffer's file \(if available\),
but can use the optional FILE-NAME instead.  For the special case of a
\"*perlnow select test*\" buffer, it works with a file name extracted
from the buffer."
  ;; Two important cases to cover are:
  ;;   ~/perldev/Horror-Grossout/lib/Horror/Grossout.pm
  ;;   ~/perldev/Horror-Grossout/t/Horror-Grossout.t
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-find-cpan-style-staging-area"))
  (let* (
         return
         ;; args for directory-files function:
         (dir        "")        ;; candidate directory under examination
         (full-names nil)
         (pattern    "^[ltMB]") ;; to pre-screen listing for interesting results only
                                ;;    lib, t, Makefile.PL, Build.PL, etc
         (nosort     t  )
         (file-list  () )       ;; file listing of the candidate directory (pre-screened)
         ;;
         (buffy (buffer-file-name))
         (pnts-file (perlnow-select-read-full-file-name)) ;; nil if not select test buffer
         (input-file
          (or file-name buffy pnts-file perlnow-associated-code perlnow-recent-pick-global))
         )
    (cond (input-file
           (setq dir (perlnow-fixdir (file-name-directory input-file)))
           ;; Look at dir, and each level above it, stepping up one each time,
           ;; give up when dir is so short we must be at root (( TODO but: Windows?  ))
           (setq return
                 (catch 'UP
                   (while (> (length dir) 1)
                     (setq file-list (directory-files dir full-names pattern nosort))

                     (dolist (file file-list)
                       (if (or
                            (string= file "Makefile.PL")
                            (string= file "Build.PL")) ;; we found it!
                           (throw 'UP dir))
                       ) ;; end dolist

                     ;; go up a directory level
                     (setq dir (perlnow-fixdir (concat dir "..")))
                     ;; if we can't read files here, give up
                     (if (not (file-accessible-directory-p dir))
                         (throw 'UP nil))

                     ) ;; end while
                   nil ) ;; end catch, ran the gauntlet without success, so return nil
                 ) ;; end setq return
           )
          (t
           (setq return nil)))

    ;; TODO this func is supposed to *find*, why do a build as a side-effect?
    (if return ;; skip if nothing found (note, that means dir is "/")
        (perlnow-cpan-style-build dir))

    (if perlnow-debug
        (message "perlnow-find-cpan-style-staging-area return: %s" return))
    return))


(defun perlnow-cpan-style-build (staging-area)
  "Does the cpan-style build in the STAGING-AREA (but only if needed).
Specifically, this runs Makefile.PL and/or Build.PL.
Output is appended to the *perlnow-build* window."
  (if perlnow-trace (perlnow-message "Calling perlnow-cpan-style-build"))
  ;; Note: relies on naming convention, "perl *.PL" creates target "*".
  (let* ( (display-buffer-name "*perlnow-build*")
          (display-buffer)
          (builders (list "Makefile.PL" "Build.PL"))
          (return-flag nil)
          )
    (dolist (builder builders)
      (let* (
             (build-target      (file-name-sans-extension builder))
             (build-target-full (concat staging-area build-target))
             (builder-full      (concat staging-area builder))
             )
        (save-excursion
          (cond ( (not (file-newer-than-file-p build-target-full builder-full))
                  (setq display-buffer (get-buffer-create display-buffer-name))
                  (set-buffer display-buffer)
                ;; TODO instead of that set-buffer, try this:
                ;;  ;; Bring the display buffer window to the fore
                ;;  (perlnow-show-buffer-other-window
                ;;     display-buffer window-size t)
                ;;  (perlnow-blank-out-display-buffer display-buffer t)
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
      )))

(defun perlnow-get-incspot (package-name pm-location)
  "Determine the module root, the place where the package namespace begins.
Given the PACKAGE-NAME \(e.g. \"New::Module\"\),
and the PM-LOCATION \(as an absolute path to the \".pm\" file,
e.g. for \"/home/doom/perldev/Punk/Skunk/New/Module.pm\"\
the PM-LOCATION is \"/home/doom/perldev/Punk/Skunk/New/\"\),
Returns the module root, \(which in this example is:
\"/home/doom/perldev/Punk/Skunk/\"\) Returns nil if pm-location is nil."
  (if perlnow-trace (perlnow-message "Calling perlnow-get-incspot"))
  ;; Example:
  ;;  /home/doom/perldev/Punk/Skunk/New/Module.pm
  ;;  /home/doom/perldev/Punk/Skunk/New/              => number of slashes:  7
  ;;                                New::Module       => double-colon-count: 1
  ;;  /home/doom/perldev/Punk/Skunk/                  The desired incspot
  ;;
  (let (( incspot ))
    (cond ((eq pm-location nil)
           (setq incspot nil))
          (t
           ;; Conditioning pm-location: if there's a trailing .pm, strip the last level
           (if (string-match
                 (concat "^\(.*?" perlnow-slash "\).*?\\.pm$") pm-location)
               (setq pm-location (match-string 1 pm-location)))
           ;; Ensure there's a trailing slash (among other things)
           (setq pm-location (perlnow-fixdir pm-location))

           (let* ((module-terms-list (split-string package-name "::"))
                  (rev-module-terms-list (reverse module-terms-list))
                  (pattern)
                  )
             (pop rev-module-terms-list) ;; discard lowest level (the *.pm)
             (setq incspot pm-location) ;; will trim terms from end, and return
             (dolist (term rev-module-terms-list)
               (setq pattern
                     (concat "^\\(.*?" perlnow-slash "\\)"
                             term
                             perlnow-slash
                             "$"))
               (if (string-match pattern incspot)
                   (setq incspot (match-string 1 incspot))
                 (error "%s from %s not found in expected place in %s"
                        term package-name pm-location)
                 )
               ))))
    incspot))

;; TODO what I'm calling the "new form" here may be
;;      named "v-strings", and they have been going
;;      in-and-out of style.
(defun perlnow-perlversion-old-to-new (given-version)
  "Convert old form of perl version into the new form.
For example, a GIVEN-VERSION might be 5.006 for which the new is 5.6.0
which is more suitable for use as the -b parameter of h2xs.
If given a version that is already in the new style, just
passes it through unchanged."
  (if perlnow-trace (perlnow-message "Calling perlnow-perlversion-old-to-new"))
  ;; TODO -- the regexps here probably need improvement.
  ;; Get a definitive list of cases of perl versions that it
  ;; should handle, write a unit test, and refactor this
  (let ((old-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)$")
        (new-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9]*\\)\\.\\([0-9][0-9]*\\)")
        major
        mantissa
        minor1)
    (cond
     ((string-match new-version-pat given-version)
       given-version )
     ((string-match old-version-pat given-version)
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
  (if perlnow-trace (perlnow-message "Calling perlnow-staging-area"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-full-path-to-cpan-style-module"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-full-path-to-dev-test-file"))
  (let (  (module-test-location "")
          (test-file1 "")     ; new-style, e.g.      New-Module.t
          (test-file2 "")     ; strange beast, e.g.  Module.t
          (test-file3 "1.t")  ; old-style numeric file name
          (test-file "")      ; returned value
          (basename "")
          (basename-truncated "")
          )
    (setq staging-area (perlnow-fixdir staging-area))
    (setq module-test-location
          (concat staging-area "t/"))
    ;; peel off the lower level of "staging-area",
    ;; to get the probable base-name
    (let ((dir staging-area))
      ;;      (string-match "\\(^.*/\\)\\([^/]*\\)[/]*$" dir)
      ;;; TODO - regexp has unix slash dependency
      (string-match "\\(^.*/\\)\\([^/]*\\)/$" dir)
      (setq basename (match-string 2 dir))
      (unless basename
        (message "warning: blank basename found in perlnow-full-path-to-dev-test-file"))
      )
    (setq test-file1 (concat module-test-location basename ".t"))
    ;; for the hell of it, peel off the last part
    ;; of that name, a second try for basename (not likely)
    (string-match "\\(^.*-\\)\\([^-]*\\)$" basename)
    (setq basename-truncated (match-string 2 basename))
    (setq test-file2 (concat module-test-location basename-truncated ".t"))
    ;; And failing that, well try the numeric name, 1.t
    ;; And if *that* fails, we'll return the directory location
    ;; (a feature that might be better than just returning a
    ;; single file, eh?  Maybe should only open the h2xs test file
    ;; when there's only one there...  Think about that -- TODO).
    (cond ((file-exists-p test-file1)
           (setq test-file test-file1 ) )
          ((file-exists-p test-file2)
           (setq test-file test-file2 ) )
          ((file-exists-p test-file3)
           (setq test-file test-file3 ) )
          ((file-directory-p module-test-location)
           (setq test-file module-test-location))
                 ;; would that work, returning a directory?
          (t
           (error "Can't find h2xs test file or test location")
           ))
    test-file))

;; Essentially an efficiency hack
(defun perlnow-full-path-new-module-starter-test-file (modstar-location package-name)
  "Get the full path to a the new test file to be added to a
structure created by module_starter (using Module::Build).
Follows a very simple fixed policy, given a module named
Modular::Stuff creates a file called 01-Modular-Stuff.t."
  (if perlnow-trace (perlnow-message "Calling perlnow-full-path-new-module-starter-test-file"))
  (let* (
         (hyphenated (mapconcat 'identity (split-string package-name "::") "-"))
         (location (concat (perlnow-fixdir modstar-location) "t" perlnow-slash))
         (filename (format "01-%s.t" hyphenated))
         (fullname (concat location filename))
         )
    fullname))

(defun perlnow-export-list-for (package-name &optional incspot-opt)
  "Gets the full export list for the given PACKAGE-NAME.
The package should be located in the given INCSPOT-OPT, though
if the module is installed, that can be omitted."
;; Generates and runs shell commands like:
;;  perl -I'/home/doom/lib' -MCranky::Devil -e 'print join " ", @Cranky::Devil::EXPORT_OK, "\n";' 2>/dev/null
  (let* ((incspot
          (cond (incspot-opt)
                (t
                 (perlnow-get-incspot
                  package-name
                  (file-name-directory
                   (perlnow-module-found-in-INC package-name))))))
         (cmd1  ;; gets @EXPORT_OK
          (concat
           "perl"
           " -I'"
           incspot
           "' -M"
           package-name
           " -e 'print join \"\t\", @"
           package-name
           "::EXPORT_OK, \"\n\";' 2>/dev/null"))
         (export-ok-str  (shell-command-to-string cmd1 ))
         (export-ok-list (split-string export-ok-str "\t" t "[ \t\n]"))
         (cmd2  ;; gets @EXPORT
          (concat
           "perl"
           " -I'"
           incspot
           "' -M"
           package-name
           " -e 'print join \"\t\", @"
           package-name
           "::EXPORT, \"\n\";' 2>/dev/null"))
         (export-main-str  (shell-command-to-string cmd2 ))
         (export-main-list (split-string export-main-str "\t" t "[ \t\n]"))
         (export-list (append export-main-list export-ok-list))
         )
    export-list))
;;(perlnow-export-list-for "Cranky::Devil" "/home/doom/lib")

(defun perlnow-inc ()
  "Returns contents of perl's @INC as a list."
  (let* ((perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_\t\"}'" ))
         (inc-path-list (split-string perl-inc "\t" t "[ \t\n]"))
         )
    inc-path-list))

(defun perlnow-perl5lib ()
  "Returns contents of the PERL5LIB envar @INC as a list."
  (let* ((sep path-separator)  ;; on unix, ":"
         (perl5lib-list (split-string (getenv "PERL5LIB") sep))
         )
    perl5lib-list))

;; TODO could refactor to use perlnow-inc
(defun perlnow-incspot-in-INC-p (&optional incspot)
  "Determine if the INC-SPOT has been included in perl's @INC search path.
If not given a INC-SPOT, it defaults to using the module root of the
current file buffer.  Used by \\[perlnow-do-script-from-module]."
  (if perlnow-trace (perlnow-message "Calling perlnow-incspot-in-INC-p"))
  ;; Note: Just checking getenv("PERL5LIB") would be close, but
  ;; using @INC as reported by perl seems more solid, so that's
  ;; what we do here.
  (unless incspot
    (setq incspot
          (perlnow-get-incspot
           (perlnow-get-package-name-from-module-buffer)
           (file-name-directory (buffer-file-name)))))
  (let* (
         (perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_\t\"}'" ))
         (inc-path-list (split-string perl-inc "\t" t "[ \t\n]"))
         return )
    (setq return
          (catch 'UP
            (dolist (path inc-path-list)
              (if (string= path incspot)
                  (throw 'UP t)))))
    return))

(defun perlnow-module-found-in-INC (package-name)
  "Given a perl PACKAGE-NAME \(in double-colon separated form\)
return the first module file location found in perl's @INC
array, or nil if it is not found."
  (if perlnow-trace (perlnow-message "Calling perlnow-module-found-in-INC"))
  (let* ((full)
         (retval)
         (module-file-tail
          (concat
           (replace-regexp-in-string "::" perlnow-slash package-name)
           ".pm"))
         (inc-path-list (perlnow-all-incspots))
         )
    (setq retval
          (catch 'TANTRUM
            (dolist (inc-path inc-path-list)
              (setq full (concat (perlnow-fixdir inc-path) module-file-tail))
              (if (file-exists-p full)
                  (throw 'TANTRUM full)))))
    retval))

(defun perlnow-all-incspots ()
  "Return a list of all locations in perl's @INC array."
  (if perlnow-trace (perlnow-message "Calling perlnow-all-incspots"))
  (let* ((perl-inc
          (shell-command-to-string
           "perl -e 'print join \"\t\", @INC '" ))
         ;; omit-nulls & trim whitespace
         (inc-path-list (split-string perl-inc "\t" t "[ \t\n]"))
         )
    inc-path-list))







;;;==========================================================
;;; Read perlmodule path and names in one step
;;; (used by perlnow-module)
;;;
;;; perlnow-prompt-for-new-module-in-one-step and relatives
;;; are a variant of the old perlnow-prompt-for-module-to-create.
;;;
;;; Instead of completing-read this uses read-from-minibuffer
;;; with a customized keymap that totally transforms it's behavior.
;;;
;;; For a discussion of the following code, see this article:
;;;   http://obsidianrook.com/devnotes/elisp-prompt-new-file-part3.html
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
  (if perlnow-trace (perlnow-message "Calling perlnow-read-minibuffer-complete"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-read-minibuffer-complete-word"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-read-minibuffer-workhorse"))
  (let ( ;; empty declarations:
        raw_string candidate-alist suggested-completion field-start
        word-separator two-pieces-list perlish-path fragment fragment-pat
        file-system-path lastchar returned new-portion new-portion-first-word
        result new-mini
        ;; definitions
        (end-of-prompt-pat ": ")
        (pm-extension-pat "\\.pm$"))
    (setq raw_string (buffer-string))
    (string-match end-of-prompt-pat raw_string)
    (setq field-start (match-end 0)) ; also used later to blank minibuffer
    (setq minibuffer-string (substring raw_string field-start))
    ;; No single trailing colons allowed: double them up
    (if (string-match "[^:]:$" minibuffer-string)
        (setq new-mini (concat minibuffer-string ":"))
      (progn ;; else, do usual processing
             ;; Treat input string as a directory plus fragment
        (setq two-pieces-list
              (perlnow-divide-module-path-dir-and-tail minibuffer-string))
        (setq perlish-path (car two-pieces-list))
        (setq fragment (cadr two-pieces-list))
        (setq fragment-pat (concat "^" fragment))
        (cond (;; Are we inside the perl package namespace yet?
               (string-match "::" perlish-path)
               (setq file-system-path
                     (replace-regexp-in-string "::" perlnow-slash perlish-path))
               (setq separator "::"))
              (t
               (setq separator perlnow-slash)
               (setq file-system-path perlish-path)))
        (setq candidate-alist
              (perlnow-modules-and-dirs-alist
                file-system-path fragment-pat))
        (setq returned (try-completion fragment candidate-alist))
        ;; must convert logical values of "returned" into appropriate strings
        (cond ((eq returned nil)
               (setq suggested-completion fragment))
              (;; a precise match that is not a *.pm file is a directory: add separator
               (eq returned t)
               (if (string-match pm-extension-pat fragment)
                   (setq suggested-completion (substring fragment 0 (match-beginning 0) ))
                 (setq suggested-completion (concat fragment separator))))
              (t
               (setq suggested-completion returned)))
        ;; Prevents .pm extensions from appearing in the minibuffer
        ;; (Yeah, checking *again*. Inelegant, but WTH)
        (if (string-match pm-extension-pat suggested-completion)
            (setq suggested-completion (substring suggested-completion 0
                                                  (match-beginning 0) )))
        ;; if there's no change from the input value, go into help
        (setq result (concat perlish-path suggested-completion))
        (if (string= result minibuffer-string)
            (perlnow-read-minibuffer-completion-help))
        ;; peel off existing fragment from suggested-completion,
        ;; what remains is the new-portion
        (string-match fragment-pat suggested-completion)
        (setq new-portion (substring suggested-completion (match-end 0)))
        (if restrict-to-word-completion  ;; for "spacey"
            (progn ;; peel off word from the new-portion of suggested-completion
              (string-match "\\(^\\w*\\)\\(\\W\\|$\\)" new-portion)
              (setq new-portion-first-word
                    (match-string 1 new-portion))
              (setq word-separator
                    (match-string 2 new-portion))
              ;; When new-portion-first-word is empty, we're at a word-separator
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
  (if perlnow-trace (perlnow-message "Calling perlnow-read-minibuffer-completion-help"))
  (let* ((raw_string (buffer-substring-no-properties (point-min) (point-max)))
         (pat ": ")
         (field-start (+ (string-match pat raw_string) (length pat)))
         (string (substring raw_string field-start))
         ;; Treat input string as a directory plus fragment
         (two-pieces-list
           (perlnow-divide-module-path-dir-and-tail string))
         (perlish-path (car two-pieces-list))
         (fragment (cadr two-pieces-list))
         (fragment-pat (concat "^" fragment))
              ;; for getting possible filename completions
         ;; out of a list of bare filenames (no path)
         (file-system-path
          (replace-regexp-in-string "::" perlnow-slash perlish-path))
            ;; file system separator "/" replaces perl package separators "::"
         (match-alist) )
    (setq match-alist
          (perlnow-modules-and-dirs-alist
           file-system-path
           fragment-pat))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-remove-pm-extensions-from-alist"))
  ;; Does the numbering of items in the alist matter one way or another?
  (let (name new-alist (i (length alist)) )
    (dolist (pair alist)
      (setq name (car pair))
      (setq name (replace-regexp-in-string "\\.pm$" "" name))
      (setq new-alist (cons (cons name i) new-alist))
      (setq i (- i 1))
      )
    (setq new-alist (reverse new-alist))
    ))


(defun perlnow-modules-and-dirs-alist (file-system-path pattern)
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
  (if perlnow-trace (perlnow-message "Calling perlnow-modules-and-dirs-alist"))
;;; For extra credit how about stripping the .pm on the file names?
;;; Nope: I can't do that, it messes up "workhorse" as written.
  (let* ((match-alist)
         ;; some directory-files arguments:
         (directory-full-name nil)
         (directory-nosort nil)
         (file-list
          (directory-files
             file-system-path
             directory-full-name
             pattern
             directory-nosort))
         (i 1)  ;; counter to build alist with numeric value
         )
    (dolist (file file-list)
      (if (perlnow-interesting-file-name-p file)
          (cond ((file-directory-p (concat file-system-path file))
                 (setq match-alist (cons (cons file i) match-alist))
                 (setq i (+ i 1)))
                ((string-match "\\.pm$" file)
                 (setq match-alist (cons (cons file i) match-alist))
                 (setq i (+ i 1))))))
    ;; Reverse the order to get values counting up starting from 1
    (setq match-alist (reverse match-alist))
    ))

;;; TODO
;;; Fix any portability problem here.  Can pattern [^/] work on windows?
;;; Why not build it up using perlnow-slash?
;;; Possibly better: don't use regexps here
;;;  o  string search for last perlnow-slash, use subdir to get b4 and after
(defun perlnow-divide-hybrid-path-and-package-name (string)
  "Divide the hybrid form of a module path into the two components.
Input STRING is expected to be a hybrid file system
path using slashes for the module root name space, and
double colons for the package name space inside of that.
This routine divides it into it's two components, the module root
and module name, which are returned as a two-element list."
  (if perlnow-trace (perlnow-message "Calling perlnow-divide-hybrid-path-and-package-name"))
  (let* ( (pattern
           (concat
            "^\\(.*\\)"       ; ^(.*)    - the module root (incspot)
            perlnow-slash     ; /        - the right-most slash
            "\\([^/]*\\)"     ; ([^/]*)  - module name
            "\\(\\.pm\\)*$"   ; (\.pm)*$ - the end (or an optional .pm)
            ))
          incspot
          package-name
          )
    (cond ((string-match pattern string)
           (setq incspot     (match-string 1 string))
           (setq package-name (match-string 2 string)))
          (t
           (message "Could not separate into module root and name: %s" string)))
    (list incspot package-name)))

(defun perlnow-interesting-file-name-p (string)
  "Is the given file \(or directory name\) be interesting?
Takes a bare filename (sans path) as the STRING
argument and returns t if it doesn't match the list of
uninteresting filenames patterns, otherwise nil."
  (if perlnow-trace (perlnow-message "Calling perlnow-interesting-file-name-p"))
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

;;-------
;; file and directory manipulation

(defun perlnow-divide-module-path-dir-and-tail (string)
  "Split a file system path into directory and trailing name fragment.
Allows for the use of perl's double-colon package
name separators in addition to the usual unix-like slash
character.\n
Simple example: given the STRING \"/home/doom/lib/Stri\" should return
 \"/home/doom/lib/\" and \"Stri\"\n
Perl package example: given \"/home/doom/lib/Taxed::Reb\" should return
 \"/home/doom/lib/Taxed::\" and \"Reb\"\n"
  (if perlnow-trace (perlnow-message "Calling perlnow-divide-module-path-dir-and-tail"))
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

;; general utility
(defun perlnow-ensure-directory-exists (dir &optional ask-message)
  "Make sure that given DIR exists, asking if it needs to be created.
Typically DIR should contain the full path to the the directory.
The ASK-MESSAGE default assumes that DIR is a test file location,
but an alternative one can be supplied.
This 'ask' behavior will be suppressed when `perlnow-quiet' is set."
  (if perlnow-trace (perlnow-message "Calling perlnow-ensure-directory-exists"))
  (cond ((not (file-directory-p dir))
         (cond (perlnow-quiet
                (perlnow-mkpath dir))
               (t ;; ask before creating
                (if (y-or-n-p
                     (format "Create directory: %s"
                             dir "?"))
                    (perlnow-mkpath dir))
                )))))

;; general utility
(defun perlnow-ensure-file-exists (file template)
  "If the given FILE doesn't exist, creates it using the TEMPLATE."
  (if perlnow-trace (perlnow-message "Calling perlnow-ensure-file-exists"))
  (cond ( (not (file-exists-p file))
          (let* ( (location (file-name-directory file))
                  )
            (unless (file-exists-p location)
              (make-directory location t))
            (save-excursion
              (perlnow-create-with-template file template)
              (save-buffer)
              )))))


;;--------
;; file editing (e.g. to add or remove a line from the bashrc include for perl5lib)

(defun perlnow-remove-line-from-file-regexp (file-name regexp)
  "Remove line from file FILE-NAME if it matches REGEXP."
  (let ((initial-buffer (current-buffer)) )
    (save-excursion
      (find-file file-name)
      (goto-char (point-min))
      (cond ((re-search-forward regexp nil t)
             (move-beginning-of-line 1)
             (kill-line)
             (delete-blank-lines)
             (save-buffer)
             ))
      (switch-to-buffer initial-buffer) ;; don't trust no save-excursion
      )))

(defun perlnow-add-to-file (line file-name &optional regexp)
  "Add a LINE to FILE-NAME.
If REGEXP is given, will try to add line just before line that
matches given REGEXP.  Defaults to appending at end of file."
  (let ((initial-buffer (current-buffer)) )
    (save-excursion
      (find-file file-name)
      (goto-char (point-min))
      ;; try to find match for before-spot
      (cond ((re-search-forward regexp nil t)
             (move-beginning-of-line 1)
             (previous-line 1))
            (t
             (goto-char (point-max))
             ))
      ;; chomp any trailing newline (avoid adding one redundantly)
      (setq line
            (replace-regexp-in-string (concat "\n$") "" line))
      (forward-line 1)
      (move-beginning-of-line 1)
      (open-line 1) ;; effectively, adds a "\n"
      (insert line)
      (save-buffer)
      (switch-to-buffer initial-buffer) ;; don't trust no save-excursion
      )
    ))

;; TODO use directory-files-recursively
(defun perlnow-recursive-file-listing (dir &optional regexp type)
  "Return a list of a tree of files beginning in location DIR.
The optional REGEXP can be used to filter the returned names, and
the option TYPE can be set to \"d\" to restrict the listing to
directories or to \"f\" to restrict it to ordinary files.
Note: this is a pure elisp implementation, without dependency on
anything like an external \"find\" command."
  (interactive "D")
  (if perlnow-trace (perlnow-message "Calling perlnow-recursive-file-listing"))
  (let* ((new-list))
    (let* ((full-name    t)
           (match-regexp nil)
           (nosort       t)
           (list-dirs (directory-files dir full-name match-regexp nosort))
           )
      ;; hack: pre-process to screen out "." and ".."
      (let ((clean-list))
            (mapc
             (lambda (item-d)
               (if (not (string-match "/\\.$\\|/\\.\\.$" item-d))
                   (setq clean-list (cons item-d clean-list))
                 )) list-dirs)
            (setq list-dirs clean-list))
      ;; go through list, add element to new-list if it matches
      ;; optional criteria
      (mapc
       (lambda (item-t)
         (if (and
               (or
                (not regexp)
                (string-match regexp item-t))
               (cond
                ((not type)
                 t)
                ((string= type "f")
                 (file-regular-p item-t)
                 )
                ((string= type "d")
                 (file-directory-p item-t)
                 )))
             (setq new-list (cons item-t new-list))
           )) list-dirs)
      ;; loop over list again (yeah, I know) and call this routine
      ;; recursively on any that are directories... append returned
      ;; results to new-list
      (mapc
        (lambda (item-r)
         (if (file-directory-p item-r)
             (setq new-list
                   (append
                    new-list
                    (perlnow-recursive-file-listing item-r regexp type)))
           )) list-dirs)
      new-list)))

(defun perlnow-file-extension (filename)
  "Returns the file extension of the given FILENAME.
\(I bet one of these has never been written before, eh?\)"
;; TODO couldn't you just use file-name-extension?
  (if perlnow-trace (perlnow-message "Calling perlnow-file-extension"))
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
    (perlnow-nth-file-path-level 0 \"$HOME/dev/Mod/Liar.pm\") ;; \"home\"
    (perlnow-nth-file-path-level 2 \"$HOME/dev/Mod/Liar.pm\") ;; \"End\"
    (perlnow-nth-file-path-level 1 \"$HOME/dev/Mod/Liar.pm\") ;; \"doom\"
    (perlnow-nth-file-path-level 4 \"$HOME/dev/Mod/Liar.pm\") ;; \"Trial\"
    (perlnow-nth-file-path-level 6 \"$HOME/dev/Mod/Liar.pm\") ;; \"Liar.pm\"

    (perlnow-nth-file-path-level -1 \"$HOME/dev/Mod/Liar.pm\") ;; \"Liar.pm\"
    (perlnow-nth-file-path-level -2 \"$HOME/dev/Mod/Liar.pm\") ;; \"Mod\"
    (perlnow-nth-file-path-level -3 \"$HOME/dev/Mod/Liar.pm\") ;; \"Trial\"
"
  (if perlnow-trace (perlnow-message "Calling perlnow-nth-file-path-level"))
  (setq location (perlnow-fixdir location))
  (let* ((slash (convert-standard-filename "/"))
         (list (split-string location slash t)) ;; omit-nulls is on
         (retval))
    (cond ( (>= level 0)
            (setq retval (nth level list)))
          ( (< level 0)
            (setq level (+ 1 level))
            (setq retval (nth (abs level) (reverse list)))))
    ))

;;------
;; list manipulation utilities

(defun perlnow-split-list-at (list before)
  "Split the given LIST at point matching BEFORE.
LIST is expected to be a list of strings.
Returns two lists, with BEFORE at the start of the second list.
If BEFORE not found in list, second list is nil."
  (let ( lead  tail  item  new )
    (catch 'IT
      (while (setq item (pop list))
        (cond ((string= item before)
               (push before list) ;; put it back if you don't use it
               (throw 'IT nil)
               ))
        (push item new)
        ))
    (if list ;; if there's anything left-over, call it the tail
        (setq tail list))
    (setq lead (reverse new))  ;; undo reversal from while/push
    (list lead tail)))


(defun perlnow-filter-list (string-list filter-regexp)
  "Filter the given list of strings (STRING-LIST) using the FILTER-REGEXP."
  (if perlnow-trace (perlnow-message "Calling perlnow-filter-list"))
  (if perlnow-debug
      (message "  perlnow-filter-list: string-list: %s filter-regexp: %s"
               (pp-to-string string-list) filter-regexp))
  (let (new-list)
    (cond (filter-regexp
           (dolist (string string-list)
             (cond ( (not (string-match filter-regexp string))
                     (setq new-list (cons string new-list))
                     )) )
           (setq new-list (reverse new-list)) )
          (t
           (setq new-list nil)
           ))
    new-list))

(defun perlnow-grep-list (string-list match-regexp)
  "Search the given list of strings (STRING-LIST) for items matching the MATCH-REGEXP."
  (if perlnow-trace (perlnow-message "Calling perlnow-grep-list"))
  (if perlnow-debug
      (message "  perlnow-grep-list: string-list: %s match-regexp: %s"
               (pp-to-string string-list) match-regexp))

  (let (new-list)
    (cond (match-regexp
           (dolist (string string-list)
             (cond ( (string-match match-regexp string)
                     (setq new-list (cons string new-list))
                     )) )
           (setq new-list (reverse new-list)) )
          (t
           (setq new-list nil)))
    new-list))

(defun perlnow-minimum-nonempty-list (list-of-lists)
  "Given a LIST-OF-LISTS returns the shortest list that still contains something.
In the event of a tie in list length, go with the one closest to the beginning of
the LIST-OF-LISTS."
  (if perlnow-trace (perlnow-message "Calling perlnow-minimum-nonempty-list"))
  (let ( (foundling  ()) ;; return value
         (nonempties ()) ;; intermediate
         min len  )
  ;; perform a first pass to filter out zero-length lists
  (dolist (this-list list-of-lists)
    (setq len (length this-list) )
    (cond ((>= len 1)
           (setq nonempties (cons this-list nonempties))
           )))
  ;; initilize with first
  (setq foundling (car nonempties))
  (setq min (length foundling))
  ;;
  (dolist (this-list nonempties)
    (setq len (length this-list))
    (cond ((< len min)
           (setq min len)
;;           (setq foundling this-list)
           )))

  (setq foundling
        (catch 'COLD
          (dolist (this-list (reverse nonempties))
            (setq len (length this-list))
            (cond ((= len min)
                   (throw 'COLD this-list)
                   )))))
  foundling))

;; TODO move this function to a utility package
;; (was used by perlnow-get-package-name-from-man)
(defun perlnow-vote-on-candidates (candidate-list)
  "Pick the most commonly occuring string from a list of strings.
The list should be given as the argument CANDIDATE-LIST,
the return value will be the string itself.  In the event of a tie
this favors the earlier occurrence in the list."
  (if perlnow-trace (perlnow-message "Calling perlnow-vote-on-candidates"))
  (let (score-alist)
    (dolist (candidate candidate-list)
      (let ((score 0))
        (dolist (compare candidate-list)
          (if (string= candidate compare)
              (setq score (+ 1 score)))
          )
        (setq score-alist (cons (cons candidate score) score-alist))))
    ;; Now find max value in score-alist, return key.
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

;; DEBUG
;; (perlnow-vote-on-candidates
;;  '("wuh" "tew" "tew" "thuree" "tew" "wuh" "wuh")) ;; returns whu (good)
;; (perlnow-vote-on-candidates '()) ;; returns nil (good)
;; (perlnow-vote-on-candidates '("wuh" "tew" "thuree")) ;; returns thuree (wrong!)

;;--------
;; plist utilities

(defun perlnow-stash-reload ( &optional json-file plist-symbol )
  "Reloads a plist from a json file.
The PLIST-SYMBOL defaults to the global: `perlnow-incspot-from-t-plist'
JSON-FILE defaults to: ~/.emacs.d/perlnow/incspot_from_t.json"
  (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
  (unless json-file    (setq json-file perlnow-incspot-from-t-json-file))

  (cond ((file-exists-p json-file)
         ;; there are multiple ways a json file can map to lisp datastructures...
         (let* ((json-object-type 'plist)  ;; default 'alist
                (json-array-type  'list)   ;; default 'vector

                ;; None of these actually work
                ;;   (json-key-type `symbol)
                ;;   (json-key-type `string)
                ;;   (json-key-type `keyword)

                (input-data (json-read-file perlnow-incspot-from-t-json-file))
                (input-data-fixed (perlnow-plist-keys-string-to-symbol input-data))
                )
           (set plist-symbol input-data-fixed)
           ))))

;; Note: this is hack to deal with the fact that I can't convince json-read-file
;; to restore my key-symbol/value-strings structure, it insists on keys as strings
(defun perlnow-plist-keys-string-to-symbol (plist)
  "Go through a plist with key strings and converting them to symbols."
  ;; Step through the list and skip the even values
  (let ( flip  new-data )
    (setq flip t)
    (setq new-data
          (mapcar
           (lambda (item)
             (cond ( flip
                     ;; TODO maybe make conversion to symbol conditional on stringp
                     (setq item (intern item)) ;; string-to-symbol
                     (setq flip nil)
                     )
                   (t ;; if not flip we flip to t
                    (setq flip t))
                   )
             item)
           plist))
    new-data))

(defun perlnow-write-plist-file ( &optional json-file plist-symbol )
  "Writes the data from plist to a json file.
The PLIST-SYMBOL defaults to the global: `perlnow-incspot-from-t-plist'
JSON-FILE defaults to: ~/.emacs.d/perlnow/incspot_from_t.json"
  (save-excursion
    (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
    (unless json-file    (setq json-file perlnow-incspot-from-t-json-file))
    (let* ((data
            (json-encode (eval plist-symbol)))
           )
      (find-file json-file)
      (widen)
      (delete-region (point-min) (point-max))
      (insert data)
      (save-buffer)
      ;; TODO close buffer, or just bury it?
      )))

;; TODO maybe, make this an optional argument: perlnow-incspot-from-t-json-file
(defun perlnow-stash-put ( keystr value &optional plist-symbol )
  "Put pair of KEYSTR and VALUE in the plist indicated by optional PLIST-SYMBOL.
The PLIST-SYMBOL defaults to the global `perlnow-incspot-from-t-plist'.
   Example:
     (perlnow-stash-put \"one\" \"alpha\" 'my-special-plist)
If KEYSTR is nil, does nothing and returns nil. If VALUE is nil,
silently converts it to an empty string."
  (let ((ret
         (cond (keystr
                (unless value (setq value ""))
                (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
                (set plist-symbol
                     (plist-put (symbol-value plist-symbol) (intern keystr) value))

                (message "PU perlnow-incspot-from-t-plist: %s" (pp-to-string perlnow-incspot-from-t-plist))

                (if perlnow-debug
                    (message "perlnow-stash-put: json: %s"
                             (pp-to-string (json-encode (eval plist-symbol)))))

                (let ((json-file perlnow-incspot-from-t-json-file)
                      )
                  (perlnow-write-plist-file json-file plist-symbol)
                  )
                )
               (t
                nil))))
    ret))

(defun perlnow-stash-lookup ( keystr &optional plist-symbol )
  "Look-up string KEYSTR in plist indicated by optional PLIST-SYMBOL.
The PLIST-SYMBOL defaults to the global `perlnow-incspot-from-t-plist'.
  Example:
  (setq value
    (perlnow-stash-lookup \"one\" 'my-special-plist)
If KEYSTR is nil, returns nil.
First tries a symbol form of the key, and if that fails tries the raw string."
;; And that, by the way, is a nasty hack to cover for json-read-file never doing
;; what I want, irrespective of the json-key-type setting
  (cond (keystr
         (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
         (let ( (value
                 (or
                  (lax-plist-get (symbol-value plist-symbol) (intern keystr))
                  (lax-plist-get (symbol-value plist-symbol) keystr)
                  )) )
           value))
        (t
         nil)))

;; This is like perl's "keys", and it's hard to believe I needed to write this.
(defun perlnow-plist-keys ( plist )
  "Return all keys of the given plist as a list of strings."
;; Step through a list and skipping the even values
  (let ( (flip t)
         (accumulator) )
    (dolist (item plist)
        (cond ( flip
                (let ( (key (symbol-name item)) ) ;; symbols-to-strings
                  (push key accumulator) )
                (setq flip nil)
                )
              (t ;; not flip
               (setq flip t))
              ))
    (reverse accumulator)))

(defun perlnow-plist-values ( plist )
  "Return all values of the given plist as a list."
;; Step through a list and skipping the odd values
  (let ( (flip nil)
         (accumulator ()) )
    (dolist (item plist accumulator)
      (if flip
          (push item accumulator) )
      (if flip
          (setq flip nil)
        (setq flip t))
      )
    (reverse accumulator)))


;;;========
;; making elisp more like perl

(defun perlnow-perlish-true-p (arg)
  "Return t if perl would call ARG true.
Checks for non-nil and non-empty string and non-zero."
  (cond (arg ;; arg is non-nil
         (cond ((stringp arg)
                (not (string= arg "")))
               ((numberp arg)
                (not (equal arg 0)))
               (t ;; some other non-nil type
                t)))
        (t   ;; arg is nil
         nil)))


;;========
;; perltidy

(defun perlnow-run-perltidy ()
  "Formats the entire buffer using perltidy.
Refuses to run if the perltidy command isn't found, or if you
aren't currently in a perl-mode.  Preserves the current location
of point."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-run-perltidy"))
  (let* ( (save-location (point))
          )
    (cond ((perlnow-perl-mode-p)
         (perlnow-run-perltidy-on-region (point-min) (point-max))
         )
        (t
         (message "Won't run perltidy: doesn't look like perl mode.")
         ))
    (goto-char save-location)
  ))

(defun perlnow-run-perltidy-on-region (start end)
  "Format the region using perltidy.
Running on the entire buffer is more reliable,
see: \\[perlnow-run-perltidy]."
  (interactive "r")
  (if perlnow-trace (perlnow-message "Calling perlnow-run-perltidy-on-region"))
  (let* ((probe
         (format "perltidy --version"))
         (probe-result (shell-command-to-string probe ))
         (perltidy-found-p (string-match "^This is perltidy" probe-result))
         (command
          (format "perltidy --standard-output --standard-error-output"
                  )) )
    (cond ( perltidy-found-p
            (shell-command-on-region start end command nil t "*error*")
            )
          (t
            (message "perltidy script not found.")
           ) )
    ))




;;=======
;; Insert boilerplate commands.
;;
;; TODO There might be some reason to use skeleton.el or tempo.el for these.
;;      Or yasnippet?
;;
;; Note: the following presume interspersed pod style.
;;
;; Ideally, there would be some way of customizing these, but then,
;; just writing your own routine is easy enough.

;; TODO needs a tail hook: probably wnat to save-buffer, then regenerate etags
(defun perlnow-insert-sub (&optional name)
  "Insert the framework for a new sub.
Adapts to context and inserts an OOP framework if this
is an OOP module, otherwise, an ordinary sub."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-sub"))
  (cond (name
         (cond ((perlnow-module-code-p)
                (cond ((perlnow-exporter-code-p)
                       (perlnow-insert-basic-sub name))
                      (t ;; presume OOP
                       (perlnow-insert-method name))))
               (t ;; presume a script
                (perlnow-insert-basic-sub name)))
         )
        (t
         (cond ((perlnow-module-code-p)
                (cond ((perlnow-exporter-code-p)
                       (call-interactively 'perlnow-insert-basic-sub))
                      (t ;; presume OOP
                       (call-interactively 'perlnow-insert-method))))
               (t ;; presume a script
                (call-interactively 'perlnow-insert-basic-sub)))
         )))

(defun perlnow-insert-method (name)
  "Insert the framework of a perl method definition"
  (interactive "sMethod name: ")
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-method"))
  (let* (
        ;; (internal-p (string-match "^_" name))
        (item-pod perlnow-sub-doc-pod) ;; "=item"
        (docstring-p (string-match "^=" item-pod))
        )
    (if docstring-p
        (insert (concat
             "\n"
             item-pod " " name "\n"
             "\n"
             "=cut" "\n"
             "\n"
             )))
    (insert (concat
             "sub " name " {" "\n"
             "  my $self = shift;" "\n"
             "\n"
             "\n"
             "}" "\n"
             ))
    (previous-line 3)
   ))

(defun perlnow-insert-basic-sub ( name )
  "Insert the framework of a basic (non-OOP) perl sub definition.
A pod block is created before the sub framework, and the NAME is
added to the module's export list (in the :all tag)."
  (interactive "ssub name: ")
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-basic-sub"))
  (let* (
        (internal-p (string-match "^_" name))
        (item-pod perlnow-sub-doc-pod) ;; "=item"
        (docstring-p (string-match "^=" item-pod))
        )
    (if docstring-p
        (insert (concat
             "\n"
             item-pod " " name "\n"
             "\n"
             "=cut" "\n"
             "\n"
             )))
    (insert (concat
             "sub " name " {" "\n"
             "  my $arg = shift;" "\n"
             "\n"
             "\n"
             "}" "\n"
             ))
    (previous-line 3)

    (unless internal-p
      (perlnow-add-export name))
    ))

(defun perlnow-add-export (subname)
  "For an Exporter-based module, adds SUBNAME to the \":all\" tag.
This depends on some features in the exporter-based template that ships
with perlnow: the %EXPORT_TAGS feature that defines 'all',
and the qw( ) list it uses."
  (save-excursion
  (let* ((initial-buffer (current-buffer))
         (initial-point  (point))
         ;; pattern to search for a line like this:
         ;;  our %EXPORT_TAGS = ( 'all' => [
         (all-tag-pattern
          "\\bEXPORT_TAGS[ \t]*=[ \t]*(.*?\\ball\\b")  ;; allows alt quotes on 'all'
         ;; parens are required on the qw( ) construct
         (open-quoted-words-pattern
          "qw(")
         )
    (cond ((perlnow-exporter-code-p)
           (goto-char (point-min))
           (re-search-forward all-tag-pattern nil t)
           ;; skip to "qw(", and from there to ")"
           (re-search-forward open-quoted-words-pattern nil t)
           (let* ((all-beg (point)))
             (backward-char 1)
             (forward-sexp)  ;; finds matching ')' for 'qw('
             (backward-char 2)

             ;; insert the subname and adjust indentation
             (open-line 1)
             (insert subname)
             (cperl-indent-command) ;; Q: what if we're using perl-mode?
                                    ;; Q: do I really care?
                                    ;; A: if you used tempo, this'd be covered
             ;; adjust the indentation of the ')' line
             (forward-line 1)
             (cperl-indent-command)

             ;; removes the blank lines that are (probably) in the initial template
             (goto-char all-beg)
             (delete-blank-lines)
             ))
          (t
           (message "perlnow-add-export should only be called on an Exporter-based module.")
           )
          )
    ;; returning from any excursions
    ;; (switch-to-buffer initial-buffer)
    ;; (goto-char initial-point)
  )))

(defun perlnow-insert-accessors (field)
  "Insert the basic framework for a perl setter and getter,
Presumes href-based objects.  Uses two variables to define the
naming convention for the accessors: \\[perlnow-getter-prefix],
and \\[perlnow-setter-prefix]."
  (interactive "sObject Attribute name: ")
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-accessors"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-new"))

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

;;========
;; template expansion definitions

(defun perlnow-insert-spaces-the-length-of-this-string (string)
  "Insert as many spaces as characters in the given STRING.
Used by the template.el expansion PNFS."
  (if perlnow-trace (perlnow-message "Calling perlnow-insert-spaces-the-length-of-this-string"))
  (insert
   (make-string (length
                 (file-name-nondirectory string)
                 ) ?\ )))

;; --------
;; date strings

(defun perlnow-american-date ()
  "Return the date in the common American format: MM/DD/YY.
Much derided though it may be.  Note: no leading zeros on MM or DD."
  (if perlnow-trace (perlnow-message "Calling perlnow-american-date"))
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
  (if perlnow-trace (perlnow-message "Calling perlnow-full-date"))
  (let* (
         (month (format-time-string "%B"))
         (day   (format "%d" (nth 3 (decode-time (current-time)))))
         (year (format-time-string "%Y"))
         (fulldate (format "%s %s, %s" month day year))
         )
    fulldate
    ;;    (message fulldate)
    ))

;;========
;; perl5lib manipulation

(defun perlnow-add-current-pm-incspot-to-perl5lib ()
  "If the current buffer is a perl module, make sure it's incspot is in @INC.
Intended to be used from a hook such as `after-save-hook'."
  (let* ((file (buffer-file-name))
         (package-name (perlnow-get-package-name-from-module-buffer)) )
    (cond (package-name
           (let* ((pm-location (file-name-directory file))
                  (incspot     (perlnow-get-incspot package-name pm-location)) )
             (perlnow-add-incspot-to-perl5lib incspot)
             )))))

(defun perlnow-add-incspot-to-perl5lib (incspot &optional before-spot)
  "Ensures the given INCSPOT is in @INC by modifying the PERL5LIB envar.
This also appends a line to the `perlnow-bashrc-include-file', which if
sourced in your .bashrc file, will make the change to PERL5LIB permanent.
Normally this just appends, but if BEFORE-SPOT is supplied, it will
try to insert just before BEFORE-SPOT."
  (if perlnow-trace (perlnow-message "Calling perlnow-add-incspot-to-perl5lib"))
  (let* ((sep path-separator)  ;; on unix, ":"
         (initial-buffer (current-buffer))
         (lib-list (perlnow-perl5lib))
         )
    (cond ((not (perlnow-incspot-in-INC-p incspot))
           (let* ((lead_and_tail
                   (perlnow-split-list-at lib-list before-spot))
                  (lead (nth 0 lead_and_tail))
                  (tail (nth 1 lead_and_tail))
                  new-lib-list
                  new-lib-str
                  )
              (setq new-lib-list (append lead (list incspot) tail))
              (perlnow-set-perl5lib new-lib-list)

           ;; add line to a bash include file that can make change permanent
              (let* ((line
                      (concat
                       "export PERL5LIB=$PERL5LIB" sep incspot ))
                     (regexp (concat sep before-spot "$")) )
                (perlnow-add-to-file line  perlnow-bashrc-include-file regexp) )
              )))))

(defun perlnow-remove-incspot-from-perl5lib (incspot)
  "Given an INCSPOT, removes it from the current PERL5LIB if it's found there.
Manipulates just the PERL5LIB envar, and so can't effect other
locations in @INC.  Any line for the INCSPOT found in
`perlnow-bashrc-include-file' will also be removed."
  (if perlnow-trace (perlnow-message "Calling perlnow-remove-incspot-from-perl5lib"))
  (let* ((sep path-separator)   ;; on unix, ":"
         (initial-buffer (current-buffer))
         (incspot-cleaned (substring-no-properties incspot))
         (filter-regexp (concat "^" incspot-cleaned "$"))
         lib-list
         new-list
         new-lib-str
         )
    (cond ((perlnow-incspot-in-INC-p incspot-cleaned)
           (setq lib-list
                 (split-string (getenv "PERL5LIB") sep))
           (setq new-list
                 (perlnow-filter-list lib-list filter-regexp))
           (perlnow-set-perl5lib new-list)

           ;; also remove line from the bashrc include file
           (perlnow-remove-line-from-file-regexp
              perlnow-bashrc-include-file
              (concat sep incspot-cleaned "$"))
           ))
    ))

(defun perlnow-set-perl5lib (perl5lib-list)
  "Changes envar PERL5LIB to match the given list.
Deletes blank entries at beginning and end of PERL5LIB
\(I dunno where these come from, but I want them gone\)."
  (let* ((sep path-separator)   ;; on unix, ":"
         (new-lib-str
           (mapconcat 'identity perl5lib-list sep))
         )
    ;; Delete blank entries at beginning and end
    (setq new-lib-str
          (replace-regexp-in-string (concat "^" sep "*") "" new-lib-str))
    (setq new-lib-str
          (replace-regexp-in-string (concat sep "*" "$") "" new-lib-str))
    (setenv "PERL5LIB" new-lib-str t)
    ))

;;;=======
;;; cheat commands ("cheat" == automatically fix things so checks pass)

(defun perlnow-revise-export-list ()
  "Find subs defined in the module that are not yet in the
EXPORT lists, and add them to the qw() list associated with %EXPORT_TAGS."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-revise-export-list"))
  (unless (perlnow-module-code-p)
    (error "perlnow-generate-import-list expects to be run in a module buffer."))
  (let* ((sub-list    (perlnow-list-all-subs))
         (export-list ())
         (add-list    ())
         ;; Searching for a line like this:
         ;;  our %EXPORT_TAGS = ( 'all' => [
         (all-tag-pattern
          "\\bEXPORT_TAGS[ \t]*=[ \t]*(.*?\\ball\\b")  ; allowing alt quotes,
         ;; requiring first paren
         (open-quoted-words-pattern
          "qw(")
         (closing-quoted-words-pattern
          ")")
         )
    (cond ((perlnow-exporter-code-p)
         (setq export-list (perlnow-list-all-exported-symbols))
         ;; add-list = sub-list - export-list
         (setq add-list (perlnow-subtract-lists sub-list export-list))
         (save-excursion
           (goto-char (point-min))
           (re-search-forward all-tag-pattern nil t)
           ;; skip to "qw(", and from there to ")"
           (re-search-forward open-quoted-words-pattern nil t)
           (re-search-forward closing-quoted-words-pattern nil t)
           ;; capture whitespace to preserve indentation
           (backward-word 1)
           (let* ((end (point))
                  (beg)
                  (indent)
                  )
             (move-beginning-of-line 1)  ;; TODO make conditional upon add-list?
             (setq beg (point))
             (setq indent (buffer-substring-no-properties beg end))
           ;; insert the add-list
           ;; (forward-word 1)
             (move-beginning-of-line 1) ;; redundant
             (next-line 1)
             (open-line 1)  ;; TODO make conditional upon add-list?
;;             (insert (mapconcat '(lambda (addition)
             (insert (mapconcat #'(lambda (addition)
                                   (concat indent addition))
                                add-list "\n"))
           ))))))


;;;==========================================================
;;; Older (though not deprecated) user level creation commands

(defun perlnow-script-using-this-module (script)
  "Jump quickly into a new SCRIPT that uses the current module code.
If the module is not in perl's search path \(@INC\), then an
appropriate \"use lib\" statement will be added. \n
Note: if multiple packages exist in the file \\(and that's
hardly ever really done\\) then this function will see the first
package name."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (if perlnow-trace (perlnow-message "Calling perlnow-script-using-this-module"))
  (require 'template)
  (let* ( (pm-file (buffer-file-name))
          (pm-location  (file-name-directory pm-file))
          (package-name (perlnow-get-package-name-from-module-buffer))
          (incspot     (perlnow-get-incspot package-name pm-location))
          )
    (unless package-name
      (error "%s" "This file doesn't look like a perl module (no leading package line)."))
    (perlnow-do-script-from-module script package-name incspot)))

(defun perlnow-module-two-questions (incspot package-name)
  "Quickly jump into development of a new perl module.
This is an older, but simpler form that asks the user two
questions to get the INC-SPOT and the PACKAGE-NAME.  The
newer \\[perlnow-module\] uses a hybrid form to get that
information in a single question.  This function is still provided
for people who don't don't agree that that's more convenient."
  (interactive
   ;; Because default-directory is the default location for (interactive "D"),
   ;; I'm doing the interactive call in two stages: change
   ;; default-directory momentarily, then restore it. Uses dynamic scoping via "let".
   (let ((default-directory perlnow-pm-location))
     (call-interactively 'perlnow-prompt-for-module-to-create)))
  (if perlnow-trace (perlnow-message "Calling perlnow-module-two-questions"))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module incspot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)))

;;;==========================================================
;; The "simple" functions.  Older code that doesn't use template.el.
(defun perlnow-script-simple ()
  "Quickly jump into development of a new perl script.
This is a simple, though inflexible form of \\[perlnow-script].
One advantage: it does not require the template.el package."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-script-simple"))
  ;; ask the user the name of the script to create
  ;; check to see if one exists already, and if so, ask for another name
  (let ( (ask-mess "Name for the new perl script? " )
         (file-name "") )
    (while (progn
             (setq file-name
                   (read-file-name ask-mess perlnow-script-location)
                   )
             (setq ask-mess
                   "That name is already in use, use another file name: ")
             (file-exists-p file-name)))
    (find-file file-name))
  ;; Insert the hashbang, a simple header, and make the file executable:
  (perlnow-perlify-this-buffer-simple))


(defun perlnow-perlify-this-buffer-simple ()
  "Turn the current buffer into perl window \(without template.el\).
This is a simple, but inflexible, command that doesn't
require template.el.
It does three things:
   Adds the hashbang line along with a simple header,
   Makes the file executable,
   Goes into cperl-mode using font-lock-mode."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-perlify-this-buffer-simple"))
  ;; insert the hash bang line at the top of the file:
  (goto-char (point-min))
  (insert perlnow-simple-hash-bang-line)
  (insert "\n")
  (insert "# ")
  ;; now, insert a simple header, of the form:
  ;; <programname> - <author's email>
  ;;                 <timestamp>
  (let ((file-name-no-path
         (file-name-nondirectory (buffer-file-name))) )
    (insert file-name-no-path)
    (insert " - " )
    (insert user-mail-address)
    (insert "\n")
    (insert "# ")
    ;; Indent so that the date lines up under the email address:
    (let ( (i 0) )
      (while (< i (length file-name-no-path) )
        (setq i (1+ i))
        (insert " ")))
    (insert "   ")   ; extend indent passed the " - " on line above
    (insert (current-time-string))
    (insert "\n\n"))
  ;; Switch buffer to cperl-mode (whether you like it or not)
  (cperl-mode)
  ;; Turn on font-lock-mode, (if not on already)
  (if (font-lock-mode) (font-lock-mode))
  ;; (That works, though you might think it should be "if *not* font-lock".)

  ;; Make the file executable (file must exist first, so save it now)
  (save-buffer)
  (let* ((all-but-execute-mask ?\666) ; Mask to screen out executable
           (file-permissions (file-modes (buffer-file-name)))
           (new-file-permissions
            (+ (logand file-permissions all-but-execute-mask)
               perlnow-executable-setting))
           )
      (set-file-modes (buffer-file-name) new-file-permissions)
      (message "buffer is now perlified") ))




;;;=======
;;; development tools

;;;--------
;;; clear routines for some key buffer-local vars

(defun perlnow-clear-run-string ()
  "Clear this buffer's associated `perlnow-run-string'."
  (interactive)
   (setq perlnow-run-string  ""))

(defun perlnow-clear-run-string-harder ()
  "Clear this buffer's associated `perlnow-run-string-harder'."
  (interactive)
   (setq perlnow-run-string-harder  ""))

(defun perlnow-clear-associated-code ()
  "Clear this buffer's associated `perlnow-associated-code'."
  (interactive)
   (setq perlnow-associated-code  ""))

(defun perlnow-clear-recent-pick ()
  "Clear this buffer's associated `perlnow-recent-pick'."
  (interactive)
   (setq perlnow-recent-pick  ""))

;; TODO experimental
(defun perlnow-clear-recent-pick-global ()
  "Clear this buffer's associated `perlnow-recent-pick-global'."
  (interactive)
   (setq perlnow-recent-pick-global  ""))

(defun perlnow-reset-buffer-locals ()
  "Reset the key buffer-local variables."
  (interactive)
  (perlnow-clear-run-string)
  (perlnow-clear-run-string-harder)
  (perlnow-clear-associated-code)
  (perlnow-clear-recent-pick)
  (perlnow-clear-recent-pick-global)
  )

;;;--------
;;; report/dump of some key buffer-local vars

(defun perlnow-report-status-vars ()
  "Dump status report of key buffer-local variables."
  (interactive)
  (let* (( display-buffer (get-buffer-create "*perlnow*"))
         ( window-size -13 )   ;; number of lines for display-buffer
         ( mess (perlnow-vars-report-string))
         )
  ;; Bring the *perlnow* display window to the fore
  ;;   (bottom window of the frame)
  (perlnow-show-buffer-other-window display-buffer window-size t)
  (perlnow-blank-out-display-buffer display-buffer t)
  (insert mess)
  ))

(defun perlnow-vars-report-string ()
  "Returns report of current buffer's vars as a string."
  (let* ((mess
         (concat
            (format "VARS FOR BUFFER:\n   %s\n"
                    (buffer-file-name))
            (format "perlnow-perl-script-name:   %s\n"
                    perlnow-perl-script-name)
            (format "perlnow-perl-package-name:  %s\n"
                    perlnow-perl-package-name)
            (format "perlnow-script-run-string:  %s\n"
                    perlnow-script-run-string)
            (format "perlnow-module-run-string:  %s\n"
                    perlnow-module-run-string)
            (format "perlnow-run-string:         %s\n"
                    perlnow-run-string)
            (format "perlnow-run-string-harder:  %s\n"
                    perlnow-run-string-harder)
            (format "perlnow-associated-code (%s):    %s\n"
                    (pp-to-string (bufferp perlnow-associated-code))
                    perlnow-associated-code)

            (format "perlnow-associated-buffer (%s):    %s\n"
                    (pp-to-string (bufferp perlnow-associated-buffer))
                    perlnow-associated-buffer)

            (format "perlnow-recent-pick:    %s\n"
                    perlnow-recent-pick)
            (format "perlnow-recent-pick-global:    %s\n"
                    perlnow-recent-pick-global)

            (format "perlnow-incspot-from-t-plist: %s\n"
                    (pp-to-string perlnow-incspot-from-t-plist))

            (format "perlnow-script-location: %s\n" perlnow-script-location)
            (format "perlnow-pm-location: %s\n" perlnow-pm-location)
            (format "perlnow-dev-location: %s\n" perlnow-dev-location)
            )))
    mess))

;;--------
;;; trial runs of various functions (edebug entry points)

(defun perlnow-report-t-directories ()
  "Echoes output of \\[perlnow-find-t-directories] via message."
  (interactive)
  (let* (( t-list
           (perlnow-find-t-directories))
         )
    (message "%s" t-list) ;; The %s form does automatic conversion of list (pp?)
    ))

(defun perlnow-report-all-t-directories ()
  "Echoes output of \\[perlnow-find-all-t-directories] via message."
  (interactive)
  (let* (( t-list
           (perlnow-find-all-t-directories))
         )
    (message "%s" (mapconcat 'identity t-list "\n"))
    ))

(defun perlnow-report-metadata ( &optional md )
  "Trial run of \\[perlnow-metadata], now with auto-tron!"
  (interactive)
  ;; (perlnow-tron)
  (unless md (setq md (perlnow-metadata)))
  (let* (
         (testloc          (nth 0  md))
         (dotdef           (nth 1  md))
         (namestyle        (nth 2  md))
         (testloc-absolute (nth 3  md))
         (hyphenized       (nth 4  md))
         (package-name     (nth 5  md))
         (incspot          (nth 6  md))
         (buffer           (nth 7  md))
         (filename         (nth 8  md))
         (fileloc          (nth 9  md))
         (basename         (nth 10 md))
         (file-type        (nth 11 md))
         (project-type     (nth 12 md))
         (sub-name         (nth 13 md))
         )
    ;; (message "md: %s" (pp-to-string md))
    (message
         (concat
          ")}>\n"
          (format "%30s %-40s\n" "testloc: "          testloc)
          (format "%30s %-40s\n" "dotdef: "           dotdef)
          (format "%30s %-40s\n" "namestyle: "        namestyle)
          (format "%30s %-40s\n" "testloc-absolute: " testloc-absolute)
          (format "%30s %-40s\n" "hyphenized: "        hyphenized)
          (format "%30s %-40s\n" "package-name: "     package-name)
          (format "%30s %-40s\n" "incspot: "          incspot)
          (format "%30s %-40s\n" "buffer: " (pp-to-string buffer))
          (format "%30s %-40s\n" "filename: "         filename)
          (format "%30s %-40s\n" "file-location: "    fileloc)
          (format "%30s %-40s\n" "basename: "         basename)
          (format "%30s %-40s\n" "file-type: "        file-type)
          (format "%30s %-40s\n" "project-type: "     project-type)
          (format "%30s %-40s\n" "sub-name: "         sub-name)
          "<({\n"
          ))
    )
  (perlnow-troff))

(defun perlnow-report-script-p ()
  "Report whether the current buffer looks like a perl script."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-script-p)
      (message "script!")
    (message "not script, yes?")))

(defun perlnow-report-module-code-p ()
  "Report whether the current buffer looks like a perl script."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-module-code-p)
      (message "Mod, baby.")
    (message "not so mod, eh?")))

(defun perlnow-report-perl-mode-p ()
  "Report whether the current buffer is in a perl mode."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-perl-mode-p)
      (message "got a perl mode going.")
    (message "not in the mode, eh?")))

(defun perlnow-report-test-p ()
  "Report whether the current buffer looks like perl test code."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-test-p)
      (message "you little test you.")
    (message "the un-tested life: worth living?")))

(defun perlnow-report-exporter-code-p ()
  "Report whether the current buffer looks like an exporter-based module."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-exporter-code-p)
      (message "exporter!")
    (message "isolationist")))

(defun perlnow-report-perl-code-p ()
  "Report whether the current buffer looks like perl code."
  (interactive)
  (if perlnow-trace (perlnow-message "Calling perlnow-report-script-p"))
  (if (perlnow-perl-code-p)
      (message "perlish")
    (message "sadly unperlish")))

(defun perlnow-report-sub-at-point ()
   "Echoes the output from of \[[perlnow-sub-at-point]]."
   (interactive)
   (message "sub-at-point: %s" (perlnow-sub-at-point)))

(defun perlnow-report-buffer-name ()
  ""
  (interactive)
  (message "buffer-name: %s" (buffer-name))
)

(defun perlnow-report-list-test-files  ()
  "Command that lists test files associated with current buffer.
For do debugging trial runs."
  (interactive)
   (let ((testloc "../t")
         (dotdef  "incspot")
         (namestyle "")
         (fullpath-opt nil)
         )

     (message "perlnow-list-test-files: %s"
              (pp-to-string
               (perlnow-list-test-files testloc dotdef namestyle fullpath-opt)
               ))
     ))


(defun perlnow-run-perlnow-add-incspot-to-perl5lib ()
  ""
  (interactive)
  (perlnow-add-incspot-to-perl5lib))

(defun perlnow-wtf ()
  ""
  (interactive)

  (let* (( testloc-absolute "/home/doom/tmp/t" )
         ( incspot "/home/doom/tmp/lib")
         )

    ;; EXPERIMENTAL
    (perlnow-stash-put testloc-absolute incspot)

    (message "perlnow-incspot-from-t-plist: %s\n"
             (pp-to-string perlnow-incspot-from-t-plist))
    ))



;;-------
;; debugging routines

(defun perlnow-tron ()
  (interactive)
  "Turns on trace and debug and writes a marker in *Messages*."
  (message "vvv %d vvv\n" perlnow-counter)
  (setq perlnow-trace t)
  (setq perlnow-debug t)
  (setq perlnow-counter (1+ perlnow-counter))
)

(defun perlnow-troff ()
  "Turns off trace and debug, writes a closing marker in *Messages*."
  (interactive)
  (message "^^^\n")
  (setq perlnow-trace nil)
  (setq perlnow-debug nil)
  ;; (setq perlnow-counter 0) ;; ... but I think not.
)

;; The idea here is you can edit the debug addition on the fly,
;; effectively there's a hook here for debug dumps at the
;; start of every routine.
(defun perlnow-message (&optional mess)
  "Pass along MESS to *Messages*, but with debug extras."
  (if mess (message "%s" mess))
  (cond (perlnow-debug
         (let ( (fileosity (buffer-file-name))
                more-fileishness
                bufferism
                )
;;            (if fileosity
;;                (message "Looking at: %s" fileosity))

;;           (other-window 1)
           ;; (setq more-fileishness (buffer-file-name))
;;           (setq bufferism (pp-to-string (current-buffer)))
;;           (other-window -1)
;;            (if more-fileishness
;;                (message "Other window:: %s" more-fileishness))
;;            )
;;           (message "Other window:: %s" bufferism)
;;            (message "Other window: %s"
;;                     (pp-to-string (current-buffer)))
           )
         )
        )
  )

(defvar perlnow-documentation-undocumented-features t
  "Undocumented features:

  o  \\[perlnow-insert-sub] also inserts a block of pod with an =item tag.
     that can be changed with `perlnow-sub-doc-pod'.

  o  \\[perlnow-insert-sub] always adds the sub name to @EXPORT_OK
     list \(on an Exporter-based module\), *except* if the sub is
     named with a leading underscore.

  o  \\[perlnow-display-inc-array] shows the locations in perl's @INC.
     By default this is bound to \"C-c \ *\".
"
)

(provide 'perlnow)


;; LEGAL

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


;;; perlnow.el ends here
