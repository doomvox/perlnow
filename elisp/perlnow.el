;; perlnow.el                Wed January   14, 2004
;;;                     Rev: Tue September 22, 2009
;;;                          Tue October   10, 2017

;;; Emacs extensions to speed development of perl code.

;; Copyright 2004, 2007, 2009, 2017 Joseph Brenner
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: perlnow.el,v 1.318 2009/12/04 09:16:23 doom Exp $
;; Keywords:
;; X-URL: http://obsidianrook.com/perlnow/

;; And thanks to:
;;  Quinn Weaver - bug fix for package names in inside-out OOP modules

;;; See LEGAL section below.

;;========
;; Commentary:
;;
;;  perlnow.el is intended to speed the development of perl code
;;  by automating some routine tasks.
;;
;;  For the documentation for this package, see the documentation for
;;  the dummy variable perlnow-documentation (and it's relatives), below.

;;; Code:
(provide 'perlnow)
(require 'cl-lib)      ;; cl-remove-duplicates
;; (require 'list-utils)  ;; list-utils-uniq
(require 'cperl-mode)
(require 'json)
;; (require 'seq)  ;; seq-util (using perlnow-uniq-list for now)

(defconst perlnow-version "1.0"
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

\\[perlnow-edit-test-file] - tries to find a test related to the
current buffer, and open it for editing.  If one is not found,
opens a new test file.

\\[perlnow-test-create] - like \\[perlnow-edit-test-file], but
doesn't search for an existing test file first.

\\[perlnow-test-create-manually] - let's you choose a test file
name.

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

Also install template.el: many features of this package depend on
template.el.  The latest version can be found at:

   http://sourceforge.net/project/showfiles.php?group_id=47369

You'll need some custom perl-oriented template.el templates \(\"*.tpl\"\)
that come with perlnow.el.  Most likely these templates should go in
~/.templates.

Add something like the following to your ~/.emacs file:

   \(require 'template\)
   \(template-initialize\)
   \(require 'perlnow\)
   \(perlnow-basic-setup\)

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

BASENAME: name of a file, sans extension: in the above example,
\"Stuff\"

FILE LOCATION \(or FILELOC\): directory where file is located,
\(here, usually a full path\), e.g. /usr/lib/perl/Modular/

MODULE NAME or PACKAGE NAME: perl's double colon separated name,
e.g. \"Modular::Stuff\".  Note: there's some potential confusion
with the word \"package\", it could refer to perl's \"package\"
keyword, or it could mean a \"cpan package\" \(i.e. a
\"tarball\"\), which may contain multiple modules.

INC SPOT or MODULE ROOT or PACKAGE ROOT: a place where perl's
package space begins \(e.g. /usr/lib/perl\). Perl's @INC is a list
of different such \"inc spots\".  These are often named \"lib\".

PROJECT ROOT or STAGING AREA: the location of directories for
modules, tests and scripts for a given project.

A staging area directory is created by cpan builder tools
\(milla, module-starter, h2xs, etc \) typically named in a
hyphenized-form of the module name under development
e.g. Modular-Stuff for Modular::Stuff.

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
current module \(or script\), often something like
01-Modular-Stuff-do_something-basic_case.t.

TEST LOCATION: place where the test script\(s\) are for
a given module \(almost always named \"t\"\).

")

(defvar perlnow-documentation-coding-standard t
  "If you use the \"perlnow standard\" for perl development, then
perlnow should work better for you \(though even if you don't
perlnow will do it's best\).

 o  perlnow, as of version 0.9 and later, presumes that you
    structure your projects with a trio of directories for
    modules, tests and scripts all directly under a project root.

                     |--- lib
     project_root ---|
                     |--- t
                     |
                     |--- script

    Modules should be placed in one directory \(typically named \"lib\"), and
    tests should be gathered in a parallel \"t\" directory \(sub-directories
    of \"t\" can be used to organize them\). Command-line scripts \(if any\)
    should be kept in a \"script\" or \"bin\" directory.

    perlnow is likely to be confused at times if you don't have
    that flat structure of a trio of directories, though some
    varaition in what they're called is allowed.  If you use an
    unusual name, you should add it to the appropriate list:

       `perlnow-lib-location-names'
       `perlnow-t-location-names'
       `perlnow-script-location-names'

    For example, if you're modules are in a directory named \"mods\":

    (setq perlnow-lib-location-names (cons \"mods\" perlnow-lib-location-names))


 o  That is similar (a sub-set, really) of cpan-style development,
    which is in general, the only widely-accepted standard for perl
    projects. You may want to use cpan-style development even when not
    working on projects for cpan e.g.

       dev/Modular-Stuff/
                         lib/
                               Modular/Stuff.pm
                           t/
                               01-Modular_Stuff-do_stuff-basic_test.t
                         script/
                               stuff_it

    Each project should probably be in it's own git repository.

 o  Initiate cpan-style project using Miyagawa's \"milla\"
    (see App::Milla on CPAN), which uses Module::Build::Tiny and Distzilla.
    Use this until you have a reason to do something else. See \\[perlnow-milla].

 o  Ideally, test files should target one sub in one module, and be named
    something like this \(for Modular::Stuff with sub do_something\):

      01-Modular-Stuff-do_something-basic_case.t

    Where the pattern is:

      <numeric prefix>-<Hyphenized Module Name>-<sub name>-<optional remark>.t

    The numeric prefix is an arbitrary (but ideally, unique) number whose
    sequence typically reflects the order of development of features \(or at
    least, the order of development of tests\).

    This is the sequence in which tests are usually run, though
    this should not be required.

 o  Scripts should typically be implemented with the main work done in
    modules: the main job of a script is to unpack and interpret arguments
    before passing them on to modules. Most testing should be implemented
    at the module level \(though perlnow has features to generate tests
    for scripts\).

    You should not turn on taint \(-T\) in your hash-bang line unless you
    know you need it: perl complains if you try to invoke such a script in
    a different way without the -T.

    Avoid getting involved in researching Getopt::* modules: just
    stick to Getopt::Long and (possibly) Getopt::Std and
    Pod::Usage and it's descendents should just be ignored: yes,
    it's good to try to stay DRY, but don't go overboard doing it.

 o  Most projects should begin with modules based on \"Moo\" with
    \"MooX::Types::MooseLike::Base\".

    \"Moose\" should be reserved for cases where you know you
    need more flexible, dynamic OOP features.

    Exporter-based modules are useful for when you know you
    won't do much with object-state.

 o  Favor the embedded-pod style, with sub documentation in a
    block of pod immediately preceeding the code for the sub.
    Usually that should be an =item block: Structure the code as
    a list of routines \(inside an \"=over\" and \"=back\" pod
    block, with a \"=head\" labeling the list\).

 o  Editorial: don't use \"this is an internal routine\" as an
    excuse to skip documentation. In fact don't think of routines as
    \"internal\" \(Internal to what? How do you know how someone
    else is going to want to use the code?\)

 o  You should be using cperl-mode, not the old default perl-mode.

")


(defvar perlnow-documentation-tutorial t
  "First, see: `perlnow-documentation-installation'.

Depending on how you configure things, you should then have easy access
\(perhaps as easy as a single keystroke of a function key\) to some quick short-
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

While you're working on the code, at any time, you can do a \"perlnow-run-check\"
\(\\[perlnow-run-check]\) to check for syntax errors and warnings.
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

If you want to use the perl debugger, I suggest using \\[perlnow-perldb], which should have better defaults than \\[perldb].

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

Or, for cpan-style, you could also run one of these directly:

 \\[perlnow-milla], \\[perlnow-h2xs], \\[perlnow-module-starter].

The first two are very similar, they just use different templates. Both ask
you for the name and location of the module you want to create in a single
prompt, asking for an answer in a hybrid form like:

  /home/hacker/perldev/lib/New::Module

Perlnow does it's best to find the current \"project root\" \(in
this example \"perldev\"\)-- and presumes the locations for
modules, scripts and tests are right under it-- if you'd rather
just tell perlnow where your project is, you can set
\\[perlnow-project-root-override].

If you don't like this single-prompt method of entering this
information, you can use the older form of this command,
\\[perlnow-module-two-questions].

If you do a \\[perlnow-run] on code that doesn't have a defined
run string yet, perlnow will will try to find a way to run it:
e.g. from a module it will look for a likely-looking test file, before
falling back on asking you how to do it.

You can create and edit test files using one of:
  \\[perlnow-edit-test-file],
  \\[perlnow-test-create],
  \\[perlnow-test-create-manually]

Next, the cpan-style approach to module development:
  `perlnow-documentation-tutorial-3-cpan-style-module-development'")

(defvar perlnow-documentation-tutorial-3-cpan-style-module-development t
  "There's a style of development oriented toward \(but not limited to\)
creating CPAN distributions which is a little diffferent from what was
discussed in : `perlnow-documentation-tutorial-2-module-development'.

Typically with perlnow you'd use \\[perlnow-cpan-module] to run a
\"builder\" command to generate the structure for a cpan-style
project.  This defaults to running \"milla\", but that can be changed
by setting the `perlnow-cpan-style' variable.

You can also just run one of these commands directly:
\\[perlnow-milla], \\[perlnow-module-starter] or \\[perlnow-h2xs].

Any of these commands will ask you two questions:
  \(1\) where do you want to put the build
  \(2\) what do you want to call this module.

Then you'll see two open windows, one showing the module file
buffer, the other showing the default test file for the module.

Next, the template naming convention:
 `perlnow-documentation-tutorial-4-template-naming-convention'")

(defvar perlnow-documentation-tutorial-4-template-naming-convention t
  "There's a convention for naming templates so that perlnow
can find appropriate ones for different cases.  For example, if
you run \\[perlnow-module-starter] using the default settings,
it will by preference use two templates named:

  TEMPLATE.perlnow-modstar-module_build-object-pm.tpl
  TEMPLATE.perlnow-modstar-module_build-object-pm-t.tpl

These template names are the most specific case:
Here \"modstar\" corresponds to \"module_starter\",
\"module_build\" means it's for a Module::Build cpan-style
distribution, and \"object\" means it's for OOP development.

A new feature with perlnow 1.0: the system tries to intelligently
fall back to simpler versions of the modules, so that you're less
likely to need to maintain multiple copies of identical
templates.

Next, `perlnow-documentation-test-file-strategies'")

(defvar perlnow-documentation-test-file-strategies t
  "Commands such as \\[perlnow-run] try to find an appropriate
test to exercise the code in the current buffer.

We can presume that test files all end with the \".t\" extension,
and they're typically organized in a directory named \"t\"
\(but see `perlnow-t-location-names'\).

It's fairly common \(and standard for cpan-style projects\)
for \"t\" to be located next to the main code directory \(named
\"lib\" for cpan-style\) where the module namespace begins:
as of the 1.0 release, this is a requirement for many perlnow
features to work.

As presently implemented (version 1.0), `perlnow-edit-test'
will find a test file that matches it's naming convention \(see
`perlnow-documentation-coding-standard'\), or alternately it will
look for the most recently modified one, and if it finds nothing it
will create a new one according to the naming convention.

The standard perlnow naming styles for test files
is like so: \"01-Modular-Stuff-sub_name.t\".

You can rename the files later manually for clarification, though
it's probably best if you just add a remark suffix:
\"01-Modular-Stuff-sub_name-very_stuffy_features.t\".

When you *know* you want to create a new test, you can use the
\\[perlnow-test-create] command \(or \\[perlnow-test-create-manually]\).

Next:
 `perlnow-documentation-7-template-expansions'")



;;;========
;;  User Options, Variables
;;;========

;;--------
;; directory utilities
;; (these are defined first so they can be used in variable definitions)
;;
(defun perlnow-fixdir (dir &optional root)
  "Fixes the DIR.
Conditions directory paths for portability and robustness.
Some examples:
 '~/tmp'             => '/home/doom/tmp/'
 '~/tmp/../bin/test' => '/home/doom/bin/test/'
Note: converts relative paths to absolute, using the current
default-directory setting, unless specified otherwise with the
ROOT option.  Note side-effect: converts the empty string into
the default-directory or the ROOT setting.   A nil argument is
treated in the same way."
  (unless dir (setq dir ""))
  (let ( return )
    (setq return
          (substitute-in-file-name
           (convert-standard-filename
            (file-name-as-directory
             (expand-file-name dir root)))))
    return))

(defun perlnow-mkpath (dir)
  "Create directory DIR (and intervening levels) if it doesn't exist."
  (unless (and dir (file-directory-p dir))
     (make-directory dir t)))


;;---------
;; user options (defcustom)
;;

(defcustom perlnow-project-root-override nil
  "If defined, this will always be used as a project root.
If you leave this set to nil, perlnow will search for an
appropriate project-root.  If that search process isn't working
for you, you can just tell us instead.")

(defcustom perlnow-pm-location-override     nil
  "Completely overrides perlnow search for pm location.")

(defcustom perlnow-script-location-override nil
  "Completely overrides perlnow search for script location.")

(defcustom perlnow-dev-location-override    nil
  "Completely overrides perlnow search for dev location.")

;; This is the "my3sons" presumption:
;;
;; Standard project structure:
;;
;;                   |--- lib
;;                   |
;;   project_root ---|
;;                   |--- t
;;                   |
;;                   |
;;                   |--- bin
;;
;; (Though perlnow allows some variation in the names of lib, t and bin.)

;; When possible, perlnow will search for a local project root to
;; use: often this is easy to find, e.g. in the case of cpan-style
;; development and/or git-controlled projects.  The following
;; hints are used in other cases.

(defcustom perlnow-project-root-from-lib "$PN_LIB_DIR/.."
  "Relationship between lib location and project root.
Psuedo-envars such as $PN_LIB_DIR are allowed in this definition
\(unlike actual envars, there's no way for a user to set these,
internally these are dynamically expanded by \\[perlnow-expand-path]\).
EXPERIMENTAL.")

(defcustom perlnow-project-root-from-script "$PN_SCRIPT_DIR/.."
  "Relationship between lib location and project root.
Psuedo-envars such as $PN_SCRIPT_DIR are allowed in this definition
\(unlike actual envars, there's no way for a user to set these,
internally these are dynamically expanded by \\[perlnow-expand-path]\).
EXPERIMENTAL.")

(defcustom perlnow-project-root-from-t "$PN_T_DIR/.."
  "Relationship between lib location and project root.
Psuedo-envars such as $PN_T_DIR are allowed in this definition
\(unlike actual envars, there's no way for a user to set these,
internally these are dynamically expanded by \\[perlnow-expand-path]\).
EXPERIMENTAL.")

(defcustom perlnow-project-root-to-dev-location "$PN_PROJECT_ROOT/.."
  "Relationship between the overall \"dev location\" and a project root.
Note: \"the dev location\" is where cpan-style projects are created.
Psuedo-envars such as $PN_PROJECT_ROOT are allowed in this definition
\(unlike actual envars, there's no way for a user to set these,
internally these are dynamically expanded by \\[perlnow-expand-path]\).
EXPERIMENTAL.")

;; Used in perlnow-project-root with perlnow-stepup-path-to-matching-name
(defcustom perlnow-script-location-names (list "script" "bin" "scripts")
  "List of likely names for script directories, in order of priority.")

;; Used in perlnow-project-root with perlnow-stepup-path-to-matching-name
(defcustom perlnow-t-location-names      (list "t")
  "List of likely names for test file directories, in order of priority.")

(defcustom perlnow-lib-location-names    (list "lib")
  "List of likely names for module lib directories, in order of priority.")

;; used as default-directory by perlnow-cpan-module and relatives
(defcustom perlnow-dev-location (perlnow-fixdir "$HOME/dev")
  "This is the fall-back default location to work on CPAN-style distributions.")

;; Note: perlnow-script-location and perlnow-pm-location are used
;; by creation commands, to resolve relative locations with read-file-name.
(defcustom perlnow-script-location
  (perlnow-fixdir (concat perlnow-dev-location "bin"))
  "This is the default location to stash new perl scripts.")

;; Used by perlnow-module, perlnow-object-module in "initial" value at prompt
(defcustom perlnow-pm-location
  (perlnow-fixdir (concat perlnow-dev-location "lib"))
  "This is the default location to stash new perl modules.")

(defcustom perlnow-executable-setting ?\110
  "The user-group-all permissions used to make a script executable.")

(defcustom perlnow-secondary-window-size 14
  "Number of lines to use for a secondary window.
E.g. the cpan-style project module creation scripts also display
a test-file in a secondary window.")

(defcustom perlnow-template-location (perlnow-fixdir "$HOME/.templates")
  "Standard location for template.el templates.")
(put 'perlnow-template-location 'risky-local-variable t)

(defcustom perlnow-perl-script-template nil
  "The template that new perl scripts will be created with.
Setting this overrides the usual search behavior. \\[perlnow-choose-template].")
(put 'perlnow-perl-script-template 'risky-local-variable t)

(defcustom perlnow-perl-module-template nil
  "The template that new perl modules will be created with.
Setting this overrides the usual search behavior. \\[perlnow-choose-template].")
(put 'perlnow-perl-module-template  'risky-local-variable t)

(defcustom perlnow-perl-object-module-template nil
  "The template that new perl object modules will be created with.
Setting this overrides the usual search behavior. \\[perlnow-choose-template].")
(put 'perlnow-perl-object-module-template  'risky-local-variable t)

(defcustom perlnow-perl-test-script-template nil
  "The template that ordinary module perl test scripts will be created with.
Setting this overrides the usual search behavior. \\[perlnow-choose-template].")
(put 'perlnow-perl-test-template  'risky-local-variable t)

(defcustom perlnow-perl-test-module-template nil
  "The template that ordinary module perl test scripts will be created with.
Setting this overrides the usual search behavior. \\[perlnow-choose-template].")
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

(defcustom perlnow-quiet t
  "Makes file creation operations just work.
Silently creates directories if needed, and overwrites if it
exists already.  Copies of old files should be preserved with an
'.OLD' suffix") ;; TODO is this checked consistently?
;; See perlnow-create-with-template, which is used by all the
;; perlnow create commands.
(put 'perlnow-quiet  'risky-local-variable t)
;; (setq perlnow-quiet nil)

(defcustom perlnow-suppress-use-lib-if-in-inc nil
  "When creating a perl file from a module we normally insert a FindBin/\"use
lib\" line pair, so the new file always has a relative path to the module.
It used to be that we would skip this if the module were already found in
@INC. Since we now manipulate PERL5LIB on-the-fly it's possible for modules
to come-and-go from @INC: there's less confusion if we always insert a \"use
lib\" line, even if it currently looks redundant.
For backwards compatibility to version 0.7 and earlier, set this to t.")
;; (setq perlnow-suppress-use-lib-if-in-inc nil)

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

(defvar perlnow-test-file-history nil
  "The minibuffer history for perl test files.")

(defvar perlnow-ack-history nil
  "The minibuffer history for the \\[perlnow-ack] command.")

(defconst perlnow-slash (convert-standard-filename "/")
  "A more portable form of the file system name separator.")

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
becomes the perl module name \(e.g. \"Module::Name\"\)
when used by \\[perlnow-module] function.

\(>>>PERL_SCRIPT_NAME<<<\)
becomes the recently used perl script name.
Used in creating tests that need to refer to the current script.

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
key had been hit. Use this *after* the line and not before.

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
5.006, version numbers looking like 5.7.0 or 5.8.2 are often
used.")

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

(defvar perlnow-script-run-string nil
  "The run string for perl scripts, used by \\[perlnow-run].
Leave this set to nil unless you want to override the heuristics
used by \\[perlnow-set-run-string] to determine the way to run
the current script.  This is a buffer local variable, i.e. it
may be set differently for different files.")
(make-variable-buffer-local 'perlnow-script-run-string)
(put 'perlnow-script-run-string  'risky-local-variable t)

(defvar perlnow-module-run-string nil
  "The run string for perl modules, used by \\[perlnow-run].
Leave this set to nil unless you want to override the heuristics
used by \\[perlnow-set-run-string] to determine the way to run
the current script.  This is a buffer local variable, i.e. it
may be set differently for different files.")
(make-variable-buffer-local 'perlnow-module-run-string)
(put 'perlnow-module-run-string  'risky-local-variable t)

(defvar perlnow-select-test-file-buffer-name "*select test file*"
  "Name of buffer to display lists of test files.")

(defvar perlnow-message-buffer-name "*perlnow*"
  "Name of buffer to display all perlnow messages.")

(defvar perlnow-message-buffer  nil
"Buffer used for general display of perlnow messaging.
 Used by \\[perlnow-shell-command] to store buffer object with
name `perlnow-message-buffer-name'")

(defvar perlnow-temp-buffer  nil
"Buffer for display of output of individual external processes.
See `perlnow-message-buffer'")

(defvar perlnow-bashrc-include-file
  (substitute-in-file-name "$HOME/.bashrc_perl5lib_add")
  "Full name of a .bashrc include file to make additions to PERL5LIB permanent.")
;; (setq perlnow-bashrc-include-file (substitute-in-file-name "$HOME/.bashrc_perl5lib_add"))

;;;========
;;; some policy settings

(defcustom perlnow-cpan-style "milla"
  "Instruct \\[perlnow-cpan-module] how to create a cpan-style project.
At present, perlnow.el supports: \"h2xs\", \"module-starter\", and \"milla\".
Defaults to milla.")

(defcustom perlnow-module-starter-builder "Module::Build"
  "Base module for a cpan-style distribution.
Used just by \\[perlnow-module-starter].
Examples: \"Module::Build\", \"Module::Install\", \"ExtUtils::MakeMaker\".")

(defcustom perlnow-module-style "object"
  "Type of module you usually prefer to create, e.g. \"object\", or \"exporter\".
Defaults to \"object\". This setting is only used by some routines, such as
\\[perlnow-milla] and \\[perlnow-module-starter], to choose a code template.
Note, there is no restriction on the allowed values here, any arbitrary
string can be used, provided you have code templates named accordingly.")

(defcustom perlnow-git-auto  t
  "If non-nil, does git check-ins automatically.
This is mainly for perlnow-milla.")

;; TODO does setting these to an empty string work if you like mutators?
(defvar perlnow-getter-prefix "get_"
  "Defines the naming convention for getters for object-oriented code.
Editorial: the default setting in perlnow.el is \"get_\", because
that's very common, but doesn't it make more sense to use no
prefix for the most common case?")

(defvar perlnow-setter-prefix "set_"
  "Defines the naming convention for setters for object-oriented code.")


;;;========
;;; external programs, names and paths

(defcustom perlnow-perl-program "perl"
  "Set this to provide a hint about your preferred perl binary.
For example, make it \"/usr/local/bin/perl\" if you would rather
use that than the system's perl.  Defaults to just \"perl\"
\(and let's the shell PATH sort it out\).  Note: this is used only in
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
Typically there's a one-to-one relationship between the location
of a project's code and its tests. It's easier to find the \"t\"
from some code than vice-versa, so when we do that, we save the relationship
to do reverse lookups later, so that, given a *.t file, we'll be able to find
the project's code.")

(defvar perlnow-etc-location
  (perlnow-fixdir (concat "$HOME" perlnow-slash ".emacs.d" perlnow-slash "perlnow"))
  "Location in ~/emacs.d for miscellanious perlnow files.")

(defvar perlnow-incspot-from-t-stash-file (concat perlnow-etc-location "incspot_from_t.json")
  "A file used to preserve the associations of t and incspot.")

;;;========
;;; test policy (obsolete test file search and creation settings)

;; As of the 1.0 release, these setting should have no effect.
;; The variables are still defined (and always will be) to prevent
;; breakage on upgrade if someone is still trying to set them.
(defcustom perlnow-test-policy-test-location   nil
  "CURRENTLY UNUSED. Test location for newly created test files.")

(defcustom perlnow-test-policy-dot-definition  nil
  "CURRENTLY UNUSED. Meaning of \".\" in `perlnow-test-policy-test-location'.
")

(defcustom perlnow-test-policy-naming-style    nil
  "CURRENTLY UNUSED. Naming style to be used in creating a new test file.")

;;;========
;;; internally used vars

(defvar perlnow-run-string nil
  "Tells \\[perlnow-run] how to run the code in a particular file buffer.
This is a buffer local variable which is set by the software,
and thus should not typically be set by the user directly.
See `perlnow-script-run-string' and `perlnow-module-run-string' instead.")
(make-variable-buffer-local 'perlnow-run-string)
(put 'perlnow-run-string  'risky-local-variable t)

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

(defvar perlnow-last-buffer-file-name ""
  "A global used to pass the last buffer-file-name to a hook.
See \\[perlnow-associate-last-with-current].")

;;--------
;; safety counter and limit for recursive routine
(defvar perlnow-recurse-count 0
  "Count of number of levels of recursion.
Used by \\[perlnow-find-dir-where-relative-path-begins]")
(defvar perlnow-recurse-limit 12
  "Limit on number of levels of recursion
Used by \\[perlnow-find-dir-where-relative-path-begins]")

;;--------
;; tracking depth of function call chain (hack)

(defvar perlnow-funclev 0
  "Function call level counter.  See \\[perlnow-open-func], \\[perlnow-close-func] & `perlnow-trace'")
(setq perlnow-funclev 0) ;; whenever you load the file, this should be zeroed out.

(defvar perlnow-funcname nil
  "The current function call.
This is set by \\[perlnow-open-func] and used a second time by
\\[perlnow-close-func], hence the globablism of the var deep in
the nastiness of this hack.")

;;;========
;; debugging routines (I'm a hacking)

(defun perlnow-open-func ( &optional mess funcname )
  "Function to call at the start of a function.
The MESS is logged to *Messages* (aka STDERR) via \\[messages],
and if FUNCNAME is provided, it will appended after MESS.

Uses the global `perlnow-funclev' to keep track of depth of call
stack to control message indentation.

Stashes the FUNCNAME in `perlnow-current-function'.

Example usage:
   (if perlnow-trace \(perlnow-open-func \"Calling perlnow-run-check\")\)
      ...
   (if perlnow-trace (perlnow-close-func))

Checking perlnow-trace before calling the function is a performance hack:
When trace is off we're still doing two additional conditional checks,
but the two additional function calls go away.

Doing things this way is a very fussy hack, and emacs is supposed
to have some tracing features like this \(built into edebug, as I
remember it\) but I don't know how to use them even now, which
says something.
"
  ;; just in case the funclev is fooked
  (cond ((not (wholenump perlnow-funclev))
         (message "funclev is negged: %d but I fix" perlnow-funclev)
         (setq perlnow-funclev 0)
         ))
  (cond ((< perlnow-funclev 0)
        (setq perlnow-funclev 0)))
  (let* ((space-man
          (concat
           (make-string perlnow-funclev ? )
           (make-string perlnow-funclev ? )))
         )
    (cond (funcname
           (if mess (message "%s%s %s" space-man mess funcname) )
           (setq perlnow-funcname funcname)
           )
          (t
           (if mess (message "%s%s" space-man mess))
           (setq perlnow-funcname nil)
           ))
    (setq perlnow-funclev (1+ perlnow-funclev))
    mess))

;; (if perlnow-trace (perlnow-close-func))
(defun perlnow-close-func ( &optional retval )
  "Use at close of functions to pop up a level"
  (cond ((not (integerp perlnow-funclev))
         (message "funclev is NOT but the HACK is on")
         (setq perlnow-funclev 0)
         ))
  (let* ((space-man
          (concat
           (make-string perlnow-funclev ? )
           (make-string perlnow-funclev ? )))
         )
    (cond (retval
           (message "%sreturning from: %s with: %s" space-man perlnow-funcname (pp-to-string retval)))
          (t
           (message "%sreturning from: %s" space-man perlnow-funcname)))
    (setq perlnow-funclev (1- perlnow-funclev))
    ;; floor at 0 (if called too many times)
    (cond ((< perlnow-funclev 0)
         (setq perlnow-funclev 0)))
    perlnow-funclev))

(defun perlnow-tron ()
  (interactive)
  "Turns on trace and debug and writes a marker in *Messages*."
  (setq debug-on-error t)
  (message "vvv %d vvv\n" perlnow-counter)
  (setq perlnow-funclev 0)
  (setq perlnow-trace t)
  (setq perlnow-debug t)
  (setq perlnow-counter (1+ perlnow-counter)))

(defun perlnow-troff ()
  "Turns off trace and debug, writes a closing marker in *Messages*."
  (interactive)
  (message "^^^\n")
  (setq perlnow-funclev 0)
  (setq perlnow-trace nil)
  (setq perlnow-debug nil)
  (setq debug-on-error nil))

(defun perlnow-cycle-trace ()
  "Cycles tron and troff, leaving tron on."
  (interactive)
  (cond (perlnow-trace
         (perlnow-troff)
         (perlnow-tron))
        (t
         (perlnow-tron)
         (perlnow-troff)
         (perlnow-tron)
         ))
  (setq perlnow-counter 0))
  
(defun perlnow-tron-if-envar ()
  "Runs \\[perlnow-tron] if envar PERLNOW_TRON is non-nil."
  (if (getenv "PERLNOW_TRON")
      (perlnow-tron)))


;;;========
;;; set-up functions

(defun perlnow-basic-setup ()
  "Do some basic setup \(e.g. create directories\) to make perlnow run smoothly.
Also see \\[perlnow-define-standard-keymappings] \(most likely, more important\)."
  (perlnow-mkpath  perlnow-script-location)
  (perlnow-mkpath  perlnow-pm-location)
  (perlnow-mkpath  perlnow-etc-location)
  (perlnow-mkpath  perlnow-dev-location)
  (perlnow-mkpath  perlnow-pm-location-override)
  (perlnow-mkpath  perlnow-script-location-override)
  (perlnow-mkpath  perlnow-dev-location-override)
  (perlnow-mkpath  perlnow-template-location)
  )

;; Getting DRYer, via some hackery with eval & read in mode hooks.
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
  ;; I also do them in the "local" section, just to make sure
  ;; they get done... "global-set" is more like default-set.
  ;; who knows what you'll really get.
  (global-set-key (format "%ss" prefix) 'perlnow-script)
  (global-set-key (format "%sm" prefix) 'perlnow-module)
  (global-set-key (format "%so" prefix) 'perlnow-object-module)
  (global-set-key (format "%sP" prefix) 'perlnow-cpan-module)

  (global-set-key (format "%st" prefix) 'perlnow-edit-test-file)
  (global-set-key (format "%sa" prefix) 'perlnow-test-create)
  (global-set-key (format "%sA" prefix) 'perlnow-test-create-manually)

  ;; These bindings can be specific to the user's favorite perl mode.
  (let ( (define-perl-bindings-string
           (replace-regexp-in-string
            "%s" prefix
            "'(lambda ()
               (local-set-key \"%sc\" 'perlnow-run-check)
               (local-set-key \"%sd\" 'perlnow-perldb)
               (local-set-key \"%sr\" 'perlnow-run)
               (local-set-key \"%sR\" 'perlnow-set-run-string)

               (local-set-key \"%sb\" 'perlnow-back-to-code)
               (local-set-key \"%s1\" 'cperl-perldoc-at-point)
               (local-set-key \"%s#\" 'comment-region)
               (local-set-key \"%si\" 'perlnow-insert-sub)

               (local-set-key \"%s*\" 'perlnow-display-inc-array)

               (local-set-key \"%ss\" 'perlnow-script)
               (local-set-key \"%sm\" 'perlnow-module)
               (local-set-key \"%so\" 'perlnow-object-module)
               (local-set-key \"%sM\" 'perlnow-cpan-module)  ;; but can just run o or m with C-u

               (local-set-key \"%sn\" 'perlnow-move-next-sub)
               (local-set-key \"%sp\" 'perlnow-move-prev-sub)

               (local-set-key \"%sN\" 'perlnow-next-todo)
               (local-set-key \"%sP\" 'perlnow-prev-todo)

               (local-set-key \"%st\" 'perlnow-edit-test-file)
               (local-set-key \"%sa\" 'perlnow-test-create)
               (local-set-key \"%sA\" 'perlnow-test-create-manually)
               ;; pick one
               ;; (local-set-key \"\C-xnd\" 'perlnow-narrow-to-defun)
               (local-set-key \"\C-xnd\" 'perlnow-narrow-to-defun-other-buffer)
               )"
            ))
         )
    (add-hook 'cperl-mode-hook (eval (read define-perl-bindings-string)))
    (add-hook 'perl-mode-hook  (eval (read define-perl-bindings-string)))
    ))

;;;========
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-run-check"))
  (widen)
  (let* ( (full-file (buffer-file-name))
          (location (file-name-directory full-file))
          (filename (file-name-nondirectory full-file))
          (default-directory location)
          (perl (perlnow-how-to-perl))

          ;; podchecker --help 2>&1 | grep 'Usage:'
          (podchecker-probe
           (concat perlnow-podchecker-program " --help 2>&1 | grep 'Usage:'"))
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
    (if perlnow-trace (perlnow-close-func))
    (compile compile-command);; this just returns buffer name "*compilation*"
    ))

;; TODO could just delete the "&optional harder-setting": does nothing
;;      ... or you might find something to use it for
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
   (let* ((harder-setting (car current-prefix-arg))
          (existing-run-string
           (cond ( harder-setting
                   perlnow-run-string-harder)
                 (t
                  perlnow-run-string)
                 ))
          (run-string
           (or
            (if (not (string= existing-run-string ""))
                existing-run-string)
            (perlnow-set-run-string harder-setting)
            )))
     (list run-string harder-setting)
     ))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-run"))
  (widen)
  ;; saving the result (*possibly* redundant, but can't hurt)
  (perlnow-sync-save-run-string run-string harder-setting)
  (if run-string
      (compile run-string))
  (if perlnow-trace (perlnow-close-func))
  )

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
  (interactive
   (let ((harder-setting (car current-prefix-arg)))
     (list harder-setting)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-set-run-string"))
  (let* ((module-p (perlnow-module-code-p)))
    (let ( run-string   ret )
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
      (setq ret
            (perlnow-sync-save-run-string run-string harder-setting))
      (if perlnow-trace (perlnow-close-func))
      ret)))

(defun perlnow-perldb (run-string)
  "Run the perl debugger on the code in this file buffer.
This uses an interactively set RUN-STRING determined from
`perlnow-run-string' which may have been set by using
\\[perlnow-set-run-string].  If `perlnow-run-string' is nil,
\\[perlnow-set-run-string] is called automatically.
It can always be changed later by running \\[perlnow-set-run-string]
manually.
An advantage this command has over running \\[perldb] directly:
you can have different `perlnow-run-string' settings for different code buffers."
  (interactive
   (let (rs)
;;     (cond((eq perlnow-run-string nil)  ;; TODO use perlnow-perlish-true-p
     (cond((perlnow-perlish-true-p perlnow-run-string) 
           (setq rs (perlnow-set-run-string)))
          (t
           (setq rs perlnow-run-string)))
     (list rs)
     ))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perldb"))
  ;; TODO make sure perldb can use rs-- e.g. a leading "cd blah;" is ng
  ;;      strip trailing redirects?
  (widen)
  (let ( modified-run-string  hacked-run-string )

    (cond ((string-match "\\bperl " run-string)
           (setq modified-run-string
                 (replace-regexp-in-string "\\bperl " "perl -d " run-string)))
          (t
           (setq modified-run-string
                 (concat "perl -d " run-string))))

    ;; TODO old dequote operation. Better: split-string-and-unquote
    (setq hacked-run-string
          (replace-regexp-in-string "'" "" modified-run-string))
    (if perlnow-trace (perlnow-close-func))
    (perldb hacked-run-string)))


(defun perlnow-ack (ack-search)
  "Does searches with the utility ack, ala grep-find.
Note: there's an ack.el package, if you'd like something fancier
than this."
  (interactive
   (let ((history 'perlnow-ack-history)
         (keymap         nil)  ;; use default keymap for grep command
         (initial-prompt nil)  ;; no initial suggestion in minibuffer
          miniread   inter-list   )
     (setq miniread
           (read-from-minibuffer
            "Do code search with ack: "
            initial-prompt keymap nil history nil nil))
     (setq inter-list (list miniread))))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-ack"))
  (widen)
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
      )))
  (if perlnow-trace (perlnow-close-func)))

;;;========
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
  ;;     (interactive
  ;;      (perlnow-prompt-user-for-file-to-create
  ;;       "Name for the new perl script? " perlnow-script-location))
  (interactive
   (let ( initial-prompt )
     (setq initial-prompt (or perlnow-script-location-override
                              (perlnow-scan-tree-for-script-loc)
                              perlnow-script-location))
     (perlnow-prompt-user-for-file-to-create "Name for the new perl script? " initial-prompt)
     )) ;; TODO would like a variant that just uses perlnow-script-location: C-u or Shift?
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script"))
  (save-restriction
    (widen)
    ;; first check the script-name for obvious errors
    (let ((initial-file (buffer-file-name)))
      (cond ((string-match "::" script-name)
             (message
              "You really don't want to create a script with a '::' do you?"))
            ((string-match "\.pm$" script-name)
             (message
              "You really don't want to create a script ending in '.pm', right?"))
            (t ;; full speed ahead
             (require 'template)
             (let (package-name)
               ;; Note: perlnow-perl-package-name is used to pass to template
               (cond
                (;; starting from module
                 (setq package-name (perlnow-get-package-name-from-module))
                 (let* ((pm-file (buffer-file-name)) ;;
                        (pm-location (file-name-directory pm-file))
                        (incspot (perlnow-get-incspot package-name pm-location)))
                   (setq perlnow-perl-package-name package-name)
                   (perlnow-do-script-from-module
                    script-name package-name incspot)
                   ))
                ;; ((perlnow-test-p)
                ;;  ;; TODO assoc new script with test?
                ;;  ;;      follow asscode to non-t code, include a use line for that?
                ;;  ;;      but: don't want to change run-string for the module,
                ;;  ;;      so "perlnow-do-script-from-module" would be no good.
                ;;  )
                (;; starting from man page
                 (setq package-name (perlnow-get-package-name-from-man))
                 (setq perlnow-perl-package-name package-name)
                 (perlnow-do-script-from-module script-name package-name))
                (t ;; no special starting place
                 (perlnow-do-script script-name))))
             ))
      (perlnow-git-add-commit-safe script-name)
      (perlnow-set-associated-code-pointers initial-file)
      (if perlnow-trace (perlnow-close-func))
      )))

(defun perlnow-module (incspot package-name  &optional harder-setting)
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
  (interactive
   (let ( initial-prompt  keymap    history
                          input   filename   return-list  )
     (setq harder-setting (car current-prefix-arg))
     (setq initial-prompt (or perlnow-pm-location-override
                              (perlnow-scan-tree-for-lib-loc)
                              perlnow-pm-location))
     ;; The keymap is key: transforms read-from-minibuffer.
     (setq keymap perlnow-read-minibuffer-map)
     (setq history 'perlnow-package-name-history)
     (setq prompt-mess-1
           (cond (harder-setting
                  "New cpan project for module \(e.g. /tmp/dev/New::Mod\): ")
                 (t
                  "New module to create \(e.g. /tmp/dev/New::Mod\): ")))
     (setq input
           (read-from-minibuffer
            prompt-mess-1
            initial-prompt keymap nil history nil nil))
     ;; remove accidentally typed ".pm"
     (setq input (replace-regexp-in-string "\.pm$" "" input))
     (setq filename
           (concat (replace-regexp-in-string "::" perlnow-slash input) ".pm"))
     (while (file-exists-p filename)
       (setq input
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
              input keymap nil history nil nil))
       (setq filename
             (concat (replace-regexp-in-string "::" perlnow-slash input) ".pm")))
     (setq return-list
           (append
            (perlnow-divide-hybrid-path-and-package-name input)
            (list harder-setting)))
     return-list)) ;; end interactive
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module"))
  (save-restriction
    (widen)
    (require 'template)
    (setq perlnow-perl-package-name package-name) ;; global to pass into template
    (save-restriction
      (widen)
      (let* ((original-file (buffer-file-name))
             (filename (perlnow-full-path-to-module incspot package-name))
             (pr (perlnow-project-root incspot))
             (default-directory incspot) ;; for benefit of perlnow-milla-p
             (module-style "exporter")
             )
      (cond (harder-setting
;;             (perlnow-cpan-module dev-location package-name)
             (perlnow-cpan-module incspot package-name) ;; EXPERIMENTAL: incspot?
             )
            ((perlnow-milla-p)
               (perlnow-milla-add pr package-name module-style)
               )
              (t
               (perlnow-create-with-template filename
                                             (perlnow-choose-module-template
                                              nil ;; cpan-style
                                              module-style
                                              perlnow-template-location
                                              ))
               (perlnow-git-add-commit-safe filename)
               ))
        (perlnow-set-associated-code-pointers original-file filename)
        (if perlnow-trace (perlnow-close-func))
        ))))

(defun perlnow-object-module (incspot package-name &optional harder-setting)
  "Quickly jump into development of a new perl OOP module.
In interactive use, gets the path INC-SPOT and PACKAGE-NAME
 with a single question, asking for an answer in a hybrid form
like so:
   /home/hacker/perldev/lib/New::Module
This works much like \\[perlnow-module], except that it uses
a different template.\n
The location for the new module defaults to the global
`perlnow-pm-location'."
  (interactive
   (let ( initial-prompt  keymap  history
          input  filename return-list  prompt-mess-1 )
     (setq harder-setting (car current-prefix-arg))
     (setq initial-prompt (or perlnow-pm-location-override
                              (perlnow-scan-tree-for-lib-loc)
                              perlnow-pm-location))
     ;; keymap is key: transforms read-from-minibuffer.
     (setq keymap perlnow-read-minibuffer-map)
     (setq history 'perlnow-package-name-history)
     (setq prompt-mess-1
           (cond (harder-setting
                  "New cpan project for OOP module \(e.g. /tmp/dev/New::Mod\): ")
                 (t
                  "New OOP module to create \(e.g. /tmp/dev/New::Mod\): ")))
     (setq input
           (read-from-minibuffer
            prompt-mess-1
            initial-prompt keymap nil history nil nil))
     (if (not (perlnow-perlish-true-p input))
         (error "perlnow-object-module: can't work without a package name"))
     ;; remove accidentally typed ".pm"
     (setq input (replace-regexp-in-string "\.pm$" "" input))
     (setq filename
           (concat (replace-regexp-in-string "::" perlnow-slash input) ".pm"))
     (while (file-exists-p filename)
       (setq input
             (read-from-minibuffer
              "This name is in use, choose another \(e.g. /tmp/dev/New::Mod\): "
              input keymap nil history nil nil))
       ;; silently ignore accidentally typed ".pm"
       (setq input (replace-regexp-in-string "\.pm$" "" input))
       (setq filename
             (concat
              (replace-regexp-in-string "::" perlnow-slash input)
              ".pm")))
     (setq return-list
           (append
            (perlnow-divide-hybrid-path-and-package-name input)
            (list harder-setting)))
     return-list)) ;; end interactive
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-object-module"))
  (save-restriction
    (widen)
    (require 'template)
    (setq perlnow-perl-package-name package-name) ; global to pass into template
    (let* (
           (original-file (buffer-file-name))  ;; TODO what if there isn't one?
           (filename (perlnow-full-path-to-module incspot package-name))
           (pr (perlnow-project-root incspot))
           (default-directory incspot) ;; for benefit of perlnow-milla-p
           (module-style "object")
           )
      (cond (harder-setting
;;             (perlnow-cpan-module dev-location package-name)
             (perlnow-cpan-module incspot package-name) ;; EXPERIMENTAL: incspot?
             )
           ((perlnow-milla-p)
             (perlnow-milla-add pr package-name module-style)
             )
            (t
             (perlnow-create-with-template filename
                                           (perlnow-choose-module-template
                                            nil ;; cpan-style
                                            module-style
                                            perlnow-template-location))

             (perlnow-git-add-commit-safe filename)
             ))
      (perlnow-set-associated-code-pointers filename original-file)
      (if perlnow-trace (perlnow-close-func))
      )))

;;--------
;; the cpan-style "builder" suite: the perlnow-cpan-module wrapper

(defun perlnow-cpan-module (dev-location package-name)
  "Quickly jump into development of a new perl CPAN-style module.
This is a wrapper function that uses the `perlnow-cpan-style' setting
to determine how to work."
  (interactive
   (let* ((projroot (perlnow-project-root)) ;; uses perlnow-current-context and perlnow-project-root-override
          (default-directory
            (or perlnow-dev-location-override
                (perlnow-expand-path-from-plist perlnow-project-root-to-dev-location
                                                (list "$PN_PROJECT_ROOT" projroot))
                perlnow-dev-location)))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-cpan-module"))
  (let* ((original-file (buffer-file-name))
         (func-str    (concat "perlnow-" perlnow-cpan-style))
         (func-symbol (read (eval func-str))))
    ;; Should be one of these (intentionally not checking this here):
    ;;   perlnow-h2xs
    ;;   perlnow-milla
    ;;   perlnow-module-starter
    (if perlnow-debug
        (message "perlnow-cpan-module cmd: %s %s %s " func-str perlnow-dev-location package-name))
    (funcall func-symbol dev-location package-name)
;; Better to do this down low, because they can be called individually:
;;     (perlnow-set-associated-code-pointers original-file)
    (if perlnow-trace (perlnow-close-func))
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
   (let* ((projroot (perlnow-project-root))  ;; uses perlnow-current-context and perlnow-project-root-override
          (default-directory
            (or perlnow-dev-location-override
                (perlnow-expand-path-from-plist perlnow-project-root-to-dev-location
                                                (list "$PN_PROJECT_ROOT" projroot))
                perlnow-dev-location)))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-h2xs"))
  (save-restriction
    (widen)
    (setq dev-location (perlnow-fixdir dev-location))
    (unless (file-exists-p dev-location)
      (make-directory dev-location t))
    (let* ( display-buffer ;; buffer object
            (h2xs-module-file  "")
            (h2xs-test-file    "")
            (h2xs-staging-area "")
            (window-size perlnow-secondary-window-size)
            )
      (setq display-buffer (get-buffer-create perlnow-message-buffer-name))
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
      (if perlnow-trace (perlnow-close-func))
      )))

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
   (let* ((projroot (perlnow-project-root))  ;; uses perlnow-current-context and perlnow-project-root-override
          (default-directory
            (or perlnow-dev-location-override
                (perlnow-expand-path-from-plist perlnow-project-root-to-dev-location
                                                (list "$PN_PROJECT_ROOT" projroot))
                perlnow-dev-location)))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-starter"))
  (save-restriction
    (widen)
    (setq cpan-location (perlnow-fixdir cpan-location))
    (unless (file-exists-p cpan-location)
      (make-directory cpan-location t))
    (let* ( (cpan-style "modstar")
            (display-buffer) ;; buffer object
            (module-file  "")
            (cpan-test-file    "")
            (cpan-staging-area "")
            (cpan-t-loc        "")
            (window-size perlnow-secondary-window-size)     ;; number of lines for the *.t file buffer
            (module-style perlnow-module-style)
            )
      (setq display-buffer (get-buffer-create perlnow-message-buffer-name))

      ;;Bring the *perlnow-module-starter* display window to the fore (bottom window of the frame)
      (perlnow-show-buffer-other-window display-buffer window-size t)
      (perlnow-blank-out-display-buffer display-buffer t)

      (let* ((default-directory cpan-location)
             (modstar-cmd (perlnow-generate-module-starter-cmd  package-name cpan-location ))
             )
        (perlnow-shell-command modstar-cmd)

        (setq cpan-staging-area
              (file-name-as-directory
               (perlnow-staging-area cpan-location package-name)))
        (perlnow-cpan-style-build cpan-staging-area)
        (setq module-file
              (perlnow-full-path-to-cpan-style-module cpan-location package-name))

        (setq cpan-t-loc (file-name-as-directory (concat cpan-staging-area "t")))
        (if perlnow-debug
            (message "cpan-t-loc: %s" cpan-t-loc))

        ;; create a module and test file using appropriate templates,
        ;; and swap the module file in place of the one module-starter creates
        (let* ( t-template pm-template )
          (setq pm-template (perlnow-choose-module-template
                             cpan-style
                             module-style
                             perlnow-template-location))
          (setq t-template (perlnow-choose-module-t-template
                            cpan-style
                            module-style
                            perlnow-template-location))
          (require 'template)
          (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

          (delete-file module-file)
          (perlnow-create-with-template module-file pm-template)

          ;; clear the "t" directory, shuffling tests out of the way to "xt"
          ;; (and here I struggle for a clean git status)
          (shell-command
           (format "git rm %s" (concat cpan-t-loc "basic.t"))
           perlnow-message-buffer-name)
          (let ((dumping-grounds
                 (concat cpan-t-loc (file-name-as-directory "..") "xt")) )
            (dolist (file (directory-files cpan-t-loc t "\\\.t$" t))
              (rename-file file dumping-grounds t)))
          ;; (perlnow-git-add (concat cpan-t-loc "basic.t")) ;; hacky, ja

          (perlnow-git-commit "Move tests created by milla to xt directory.")
          ;; create and open the *.t file
          (setq cpan-test-file
                (perlnow-full-path-new-module-starter-test-file
                 cpan-staging-area package-name))
          (perlnow-open-file-other-window cpan-test-file window-size t-template t )
          (perlnow-set-associated-code-pointers cpan-test-file module-file)
          ;;        (funcall (perlnow-lookup-preferred-perl-mode))
          (if perlnow-trace (perlnow-close-func))
          )))))

(defun perlnow-generate-module-starter-cmd (module-name location)
  "Generate shell command string to run module-starter.
Creates a standard layout for development of a perl module named MODULE-NAME
in the directory LOCATION.
Get's the user's full name from the emacs function user-full-name
and the email address from the variable user-mail-address."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-generate-module-starter-cmd"))
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
    (if perlnow-trace (perlnow-close-func))
    cmd))

(defun perlnow-milla (cpan-location package-name)
  "To quickly jump into development of a new perl CPAN-style module.
Asks two questions, prompting for the CPAN-LOCATION  \(the place where
milla will create the \"staging area\"\) and the PACKAGE-NAME
\(in perl's double-colon separated package name form\)."
  ;; Because default-directory is the default location for (interactive "D"),
  ;; I'm doing the interactive call in stages: this way can change
  ;; default-directory momentarily, then restore it.
  ;; Uses the dynamic scoping of elisp's "let"
  (interactive
   (let* ((projroot (perlnow-project-root))  ;; uses perlnow-current-context and perlnow-project-root-override
          (default-directory
            (or perlnow-dev-location-override
                (perlnow-expand-path-from-plist perlnow-project-root-to-dev-location
                                                (list "$PN_PROJECT_ROOT" projroot))
                perlnow-dev-location)))
     (call-interactively 'perlnow-prompt-for-cpan-style)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-milla"))
  (save-restriction
    (widen)
    (setq cpan-location (perlnow-fixdir cpan-location))
    ;; check that milla is installed
    (cond ((not (command-installed-p "milla"))
           (error "perlnow-milla can't run: cannot find milla command.  Install Dist::Milla from CPAN.")))

    (cond ((not (command-installed-p "git"))
           (if perlnow-git-auto
               (message
                "WARNING: milla typically works with git repositories, but git not found."))
           (setq perlnow-git-auto nil)))

    (unless (file-exists-p cpan-location)
      (make-directory cpan-location t))
    (let* ( cpan-style  display-buffer  module-file
                               cpan-test-file  cpan-staging-area  cpan-t-loc  cpan-xt-loc
                               window-size  module-style  pm-template  t-template
                               milla-cmd  git-cmd
                               pm-template  t-template
                               )
      (setq cpan-style "milla")
      (setq milla-cmd (concat "milla new " package-name ))
      (setq module-style perlnow-module-style)
      (setq window-size perlnow-secondary-window-size)     ;; number of lines for the *.t file buffer

      (let* ((default-directory cpan-location))
        (perlnow-shell-command milla-cmd)

        (setq cpan-staging-area
              (file-name-as-directory
               (perlnow-staging-area cpan-location package-name)))
        (perlnow-cpan-style-build cpan-staging-area)
        (setq module-file
              (perlnow-full-path-to-cpan-style-module cpan-location package-name))
        (setq cpan-t-loc (file-name-as-directory (concat cpan-staging-area "t")))

        ;; let us be consistent with our module-starter handling,
        (setq cpan-xt-loc (file-name-as-directory (concat cpan-staging-area "xt")))
        (copy-directory cpan-t-loc cpan-xt-loc nil t t)
        (delete-directory cpan-t-loc t)
        ;; (perlnow-ensure-directory-exists cpan-t-loc)

        ;; create a module and test file using appropriate templates, and swap
        ;; in the perlnow files in place of the ones generated by the builder
        (setq pm-template (perlnow-choose-module-template
                           cpan-style
                           module-style
                           perlnow-template-location))

        (setq t-template (perlnow-choose-module-t-template
                          cpan-style
                          module-style
                          perlnow-template-location))

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

        (perlnow-set-associated-code-pointers cpan-test-file module-file)

        (perlnow-git-add module-file)
        (perlnow-git-add cpan-test-file)
        (perlnow-git-add cpan-xt-loc) ;; just being neat
        (perlnow-git-commit (format "cpan-style project for %s" package-name))
        (perlnow-dual-window-display module-file cpan-test-file)  ;; TODO also accepts buffers.
        (if perlnow-trace (perlnow-close-func))
        ))))

(defun perlnow-milla-add ( staging-area package-name module-style )
  "Adds a module to a milla/dzil cpan-style project.
Three required arguments:
  o  the STAGING-AREA -- the project root of the existing cpan-style project
  o  the PACKAGE-NAME -- the module to be created \(double-colon form\)
  o  the MODULE-STYLE -- typically 'exporter' or 'object'
"
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-milla-add"))
  ;; TODO error out if staging-area or package-name is nil?
  (setq staging-area (perlnow-fixdir staging-area))
  (unless module-style
    (setq module-style perlnow-module-style))

  ;; check that milla is installed
  (cond ((not (command-installed-p "milla"))
        (error "perlnow-milla-add can't run: cannot find milla command.  Install Dist::Milla from CPAN.")))

  (cond ((not (command-installed-p "git"))
        (if perlnow-git-auto
            (message
             "WARNING: milla typically works with git repositories, but git not found."))
        (setq perlnow-git-auto nil)))

  (let* ( cpan-style   module-file  cpan-pm-loc  pm-template  milla-cmd  git-cmd  )
    (setq cpan-style "milla")
    (setq milla-cmd (concat "milla add " package-name ))

    (let* ((default-directory staging-area))  ;; milla looks for dist.ini here
      (perlnow-shell-command milla-cmd)

      ;; (perlnow-cpan-style-build staging-area) ;; TODO needed?
      (setq cpan-pm-loc (file-name-as-directory (concat staging-area "lib" perlnow-slash)))
      (setq module-file (concat cpan-pm-loc
                                (mapconcat 'identity (split-string package-name "::") perlnow-slash)
                                ".pm"))
      ;; create a module and test file using appropriate templates, and swap
      ;; in the perlnow files in place of the ones generated by the builder
      (setq pm-template (perlnow-choose-module-template
                         cpan-style
                         module-style
                         perlnow-template-location))

      (require 'template)
      (setq perlnow-perl-package-name package-name) ;; global used to pass value into template

      (delete-file module-file)
      (perlnow-create-with-template module-file pm-template)

      (perlnow-git-add module-file)
      (perlnow-git-commit (format "Adding %s to milla cpan-style project" package-name))

      (if perlnow-trace (perlnow-close-func))
      )))

(defun perlnow-git-add (file)
  "Perfoms a git add of given FILE in current directory.
If `perlnow-git-auto' is nil, this does nothing."
  (let* ((git-cmd (format "git add %s" (shell-quote-argument file)))
         (loc (file-name-directory file))
         (default-directory loc)
         )
    (if perlnow-debug (message "git-cmd: %s" git-cmd))
    (if perlnow-debug (message "gitloc: %s" (perlnow-find-git-location file)))
    (cond (perlnow-git-auto
           (perlnow-shell-command git-cmd)
           ))
    ))

(defun perlnow-git-commit ( &optional mess all )
  " Does a a git commit in the the current directory.
 A basic perlnow commit message is used by default,
but MESS will be appended to it it is given.
If the ALL option is supplied, does a \"git commit -a\".
If `perlnow-git-auto' is nil, this does nothing."
  (let ( git-cmd  git-mess-prefix  full-mess  all-opt-str)
    (setq git-mess-prefix "Automatic git commit by perlnow")
    (cond (mess
           (setq full-mess (concat git-mess-prefix ": " mess)))
          (t
           (setq full-mess (concat git-mess-prefix))
           ))
    (cond (all
           (setq all-opt-str "-a"))
          (t
           (setq all-opt-str "")))
    (cond (perlnow-git-auto
           (setq git-cmd (format "git commit %s -m %s" all-opt-str (shell-quote-argument full-mess)))
           (if perlnow-debug (message "git-cmd: %s" git-cmd))
           (perlnow-shell-command git-cmd)
           ))
    ))


(defun command-installed-p (cmd)
  "Runs CMD with --version option to see if it's installed."
  (let* ((check-pat "\\bversion\\b")
         (probe-cmd (format "%s --version" cmd))
         (returned-str (shell-command-to-string  probe-cmd))
         (detected (string-match check-pat returned-str)) )
    detected))

;; TODO
;; Forgot to respect this
;;       (cond (perlnow-git-auto

(defun perlnow-git-add-commit-safe (file)
  "Tries to do a git add and commit on the given FILE.
If the file is not located in a git-controlled project, this
is skipped quietly.  Also, first checks if git is installed."
  (let (project-root)
  (cond ((setq project-root (perlnow-find-git-location file))
         (if perlnow-debug (message "perlnow-git-add-commit-safe found pr: %s" project-root))
         (cond ((command-installed-p "git")
                (if perlnow-debug (message "perlnow-git-add-commit-safe sees git installed"))
                (perlnow-git-add file)
                (let ((basename (file-name-nondirectory file)))
                  (perlnow-git-commit (format "Adding %s to project" basename))
                  ))
               (t
                (message
                 "WARNING: can not find git, but this is a git contolled project: %s"
                 project-root)
                ) ))
        )))


;;---------
;; edit-test and a few related functions
;; (aka the feature from hell, the Hard Part, the Serious Mess)
;;

(defun perlnow-edit-test-file (&optional harder-setting)
  "Find \(or create\) an appropriate testfile for the current perl code.
In interactive use, tries to identify a 't' directory related to
the current buffer, and if run without an argument, tries to
guess which test file is likely to be of interest.  If run with a
prefix argument \(C-u\) this opens a test select menu, allowing the
user to select a test file manually.

This function doesn't work reliably when called non-interactively,
instead you should most likely do this:

    \(perlnow-open-test-file \(perlnow-get-test-file-name\)\)

Or if testfile is known already:

    \(perlnow-open-test-file testfile\)

Or to simulate a calling prefix and open a test select menu:

    \(perlnow-edit-test-file-harder 4\)
"
  (interactive
   (let ((harder-setting (car current-prefix-arg)))
     (list harder-setting)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-edit-test-file"))
  (save-restriction
    (widen)
    (cond ((and harder-setting (> harder-setting 1))
           (perlnow-edit-test-file-harder harder-setting))
          (t
           (let ((testfile (perlnow-get-test-file-name)))
             (perlnow-open-test-file testfile)
             )))
    (if perlnow-trace (perlnow-close-func))
    ))

;; Used by: perlnow-edit-test-file, perlnow-test-create-manually, perlnow-select-create-test
(defun perlnow-open-test-file (testfile)
  "The core of the interactive function \\[perlnow-edit-test-file].
When called, presumes that the current buffer displays code
to be associated with the given TESTFILE." ;; TODO expand docstring
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-open-test-file"))
  (if perlnow-debug
      (message "DEVO: perlnow-open-test-file: testfile: %s" (pp-to-string testfile)))
  (let ( harder-setting  new-file-p
         original-code  package-name  pm-file  pm-location  incspot )
    (setq harder-setting  (car current-prefix-arg))
    (unless (perlnow-perlish-true-p testfile)
      (error "perlnow-open-test-file called with undefined testfile: %s" (pp-to-string testfile)))
    (setq new-file-p
          (not (file-exists-p testfile)))
    (perlnow-sub-name-to-var)
    (cond
     ;; TODO maybe should add a cpan-style handler here?  TODO CHOOSETEMPLATE ditto: determine cpan-style
     (;; if module
      (setq package-name (perlnow-get-package-name-from-module))
      (if perlnow-debug
          (message "WOKE: perlnow-open-test-file: in module with package: %s" package-name))
      ;; define module incspot now, before opening test file buffer
      (setq pm-file         (buffer-file-name))
      (setq pm-location     (file-name-directory pm-file))
      (setq incspot         (perlnow-get-incspot package-name pm-location))
      (setq original-code   pm-file)
      (perlnow-open-test-file-for-module testfile package-name incspot)
      )
     ;; if script
     ((perlnow-script-p)
      ;; global to pass value to template
      (setq perlnow-perl-script-name (buffer-file-name))
      (setq original-code   perlnow-perl-script-name)
      (perlnow-open-file-other-window testfile 30 (perlnow-choose-script-t-template))
      (perlnow-git-add-commit-safe testfile)
      (funcall (perlnow-lookup-preferred-perl-mode))
      (save-buffer))
     ;; if test select menu buffer (with associated pm)
     ((perlnow-test-select-menu-p)
      (setq package-name (perlnow-module-from-t-file testfile t))

      ;; (setq incspot (perlnow-stash-lookup (file-name-directory testfile)))
      (perlnow-incspot-from-t testfile)
      (setq original-code (perlnow-full-path-to-module incspot package-name))
      (perlnow-open-test-file-for-module testfile package-name incspot)
      )
     (t
      (let ((extension (file-name-extension (buffer-file-name))))
        (cond ((string= extension "t")
               (if perlnow-debug
                   (message "perlnow-open-test-file: You're already inside of a test file."))
               ;; TODO here I try to get package-name from t-file name: what if it's not there?
               (let ( hyphenized  package-name )
                 (setq hyphenized   (nth 1 (perlnow-parse-standard-t-name testfile)))
                 (setq package-name (replace-regexp-in-string "-" "::" hyphenized))
                 (setq incspot (perlnow-scan-tree-for-lib-loc))
                 (setq original-code
                       (or
                        (perlnow-follow-associations-to-non-test-code (buffer-file-name))
                        (perlnow-full-path-to-module incspot package-name)))
                 (perlnow-open-test-file-for-module testfile package-name incspot))
               )
              (t ;; context is not a perl buffer
               ;; TODO but why do I care?  I have a fullpath for a new *.t, right?
               ;;      (Oh: but not module name to insert into my standard template...)
               (message "Perlnow: Not a perl buffer.")
               )
              ))))
    (cond ((and (file-exists-p testfile)
                original-code)
           (perlnow-set-associated-code-pointers testfile original-code)))
    (perlnow-sync-save-run-string
      (perlnow-generate-run-string testfile) harder-setting)
    (if perlnow-trace (perlnow-close-func))
    ))

;; Redundant block of code moved to a function (not too much logical org here).
(defun perlnow-open-test-file-for-module (testfile package-name incspot)
  "Internal routine to open a test file for a module.
Uses the \"module-t\" template to open the given testfile, does a
git check-in if appropriate, goes into cperl-mode, munges the new
test code to find the module easily."
  (let* ((new-file-p (not (file-exists-p testfile))) )
      ;; global to pass value to template
      (setq perlnow-perl-package-name package-name)
      (perlnow-open-file-other-window testfile 30 (perlnow-choose-module-t-template))
      (perlnow-git-add-commit-safe testfile)
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
      ))

(defun perlnow-test-create (&optional testfile)
  "Create test file using automated guess."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-test-create"))
  (save-restriction
    (widen)
    (perlnow-sub-name-to-var)
    (let ((harder-setting (car current-prefix-arg)))
      (unless testfile ;; guess a default value for testfile
        (let* ((md (perlnow-metadata))
               (testloc-absolute (nth 0  md))
               (hyphenized       (nth 1  md)))
          (setq testfile
                (perlnow-new-test-file-name testloc-absolute hyphenized))
          ))
      (cond (harder-setting  ;; not the main entry point for this, but what other behavior? TODO
             ;; minibuffer entry with testfile as default
             (perlnow-test-create-manually testfile))
            (t
             (setq perlnow-recent-pick testfile)
             (setq perlnow-recent-pick-global testfile)

             (perlnow-open-test-file testfile)

             ))
      (if perlnow-trace (perlnow-close-func))
      )))

(defun perlnow-test-create-manually (&optional testfile)
  "Create test file via minibuffer entry with TESTFILE as default."  ;; TODO expand
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-test-create-manually"))
  (save-restriction
    (widen)
    (unless testfile
      ;; guess a default value for testfile
      (let* ((md (perlnow-metadata))
             (testloc-absolute (nth 0  md))
             (hyphenized       (nth 1  md)))
        (setq testfile
              (perlnow-new-test-file-name testloc-absolute hyphenized))
        ))
    (let* ((tf-input
            (read-from-minibuffer
             "Test file: "
             testfile
             nil
             nil
             (cons 'perlnow-test-file-history 2) ;; TODO double-check the 2
             )))
      (setq perlnow-recent-pick tf-input)
      (setq perlnow-recent-pick-global tf-input)
      (perlnow-open-test-file tf-input)
      (if perlnow-trace (perlnow-close-func))
      )))

;; end: perlnow-edit-test-file related functions

;;-------
;; user-level commands for navigation, etc

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-back-to-code"))
  (let* ((other-buffer perlnow-associated-code)
         (search-other-frames nil)
         ;; checks the selected frame only
         (existing-window (get-buffer-window other-buffer search-other-frames)) )
    (cond (existing-window
           (select-window existing-window))
          (t
           (if other-buffer
               (find-file other-buffer)))
          )
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-move-next-sub ()
  "Moves point to first line of code for next sub."
  (interactive)
  (let* ((sub-meta-list (perlnow-sub-after-point))
         (subname   (nth 0 sub-meta-list))
         (sub-pt    (nth 3 sub-meta-list))
         )
    (cond (sub-pt
           (goto-char sub-pt)
           (forward-line 1)
           (message "%s" subname)
           )
          (t
           (message "Already at the last sub.")))
    ))

(defun perlnow-move-prev-sub ()
  "Moves point to first line of code for next sub."
  (interactive)
  (let* ((initial-point (point))
         (sub-begin-pat "^[ \t]*sub ")     ;; perl "sub" keyword at bol
         (sub-meta-list (perlnow-range-current-sub))
         (beg           (nth 1 sub-meta-list)))
    (cond (beg
           (goto-char beg)  ;; before the pod block, if any, at the sub otherwise
           (backward-char 1))
          (t ;; not inside a sub now
           ))
    (cond ((re-search-backward sub-begin-pat nil t 1)  ;; puts you at start of "sub "
           (let* ((sub-meta-list (perlnow-sub-after-point))
                  (subname   (nth 0 sub-meta-list))
                  (sub-pt    (nth 3 sub-meta-list)) )
             (goto-char sub-pt)
             (forward-line 1)
             (message "%s" subname)
             ))
          (t
           (goto-char initial-point)
           (message "Already at the first sub.")))
    ))

(defun perlnow-next-todo ()
  "Move to next TODO or FIXME."
  (interactive)
  (let* ((case-fold-search nil)
         (todo-pat "TODO\\|FIXME\\|WTF")
         (next-word-todo-pat (concat "\\=\\(" todo-pat "\\)"))
         )
    ;; If immediately next word is a TODO, skip past it before doing another search
    (re-search-forward next-word-todo-pat nil t)
    (re-search-forward todo-pat nil t 1)  ;; puts you right after TODO
    ))

(defun perlnow-prev-todo ()
  "Move to previous TODO or FIXME."
  (interactive)
  (let* ((case-fold-search nil)
         (todo-pat "TODO\\|FIXME\\|WTF")
         (prev-word-todo-pat (concat "\\(" todo-pat "\\)\\="))
         )
    ;; If immediately prev word is a TODO, skip past it before doing another search
    (re-search-backward prev-word-todo-pat nil t)
    (re-search-backward todo-pat nil t 1)  ;; puts you right after TODO
    ))

(defun perlnow-narrow-to-defun ()
  "Narrow to the current perl sub.
Includes any blocks of pod preceding the sub definition."
  (interactive)
  (let* ((initial-point (point))
         (sub-info (perlnow-range-current-sub))
         (subname (nth 0 sub-info))
         (beg (nth 1 sub-info))
         (end (nth 2 sub-info)) )
    (narrow-to-region beg end)
    (message "Narrowed view: try \"C-x n w\" to widen.")
  ))


(defun perlnow-narrow-to-defun-other-buffer ()
  "Narrow to the current perl sub.
Includes any blocks of pod preceding the sub definition.
First does a buffer clone to view independantly in other window."
  (interactive)
  (let* ((subname      (perlnow-sub-at-point))
         (newname      (concat (buffer-name) "-" subname))
         (display-flag t)
         (norecord     nil))
    (clone-indirect-buffer-other-window newname display-flag norecord))
  (let* ((initial-point (point))
         (sub-info (perlnow-range-current-sub))
         (subname (nth 0 sub-info))
         (beg (nth 1 sub-info))
         (end (nth 2 sub-info)) )
    (narrow-to-region beg end)
  ))

;;========
;; guess run-string routines
;;   key routines for determining appropriate run-strings.

(defun perlnow-guess-run-string (&optional harder-setting)
  "Return a good guess for the perl run string for the current buffer.
If the HARDER-SETTING option is set, then it will try to run it \"harder\",
e.g. run all tests rather than just one."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-guess-run-string"))
  (let ((filename     (buffer-file-name))
        (associated   perlnow-associated-code)
        (staging-area (perlnow-find-cpan-style-staging-area))
         testfile   run-string  )
    (cond (harder-setting
           ;; the cpan-style case
           (cond (staging-area
                  (setq run-string
                        (perlnow-cpan-style-test-run-string staging-area)))
                 (t ; non-cpan-style code
                  (setq run-string
                        (perlnow-test-run-string-harder harder-setting)))))
          (t ;; harder not set, so do a standard run
           (cond
            ;; if this is a test, just run the filename
            ((perlnow-test-p filename)
             (setq run-string (perlnow-generate-run-string filename)))

            ;; if this is a script just run with the filename
            ;; TODO: if there's a *test* for the script, run that instead (?)
            ((perlnow-script-p)
             (setq run-string (perlnow-generate-run-string filename)))
            ;; if there's an associated script/test already, just use that.
            ((and (perlnow-perlish-true-p associated)
                  (or
                    (perlnow-script-file-p  associated)
                    (perlnow-test-p associated)
                    ))
             (setq run-string (perlnow-generate-run-string associated)))

            (t ;; we are a module: scrounge around for a test or script file to use
             (setq run-string
                   (perlnow-generate-run-string-and-associate
                    (perlnow-latest 
                     (list
                      (perlnow-latest
                       (perlnow-list-perl-tests default-directory))
                      (perlnow-latest 
                        (perlnow-list-perl-scripts   ;; TODO a stub, needs improvement
                           (perlnow-scan-tree-for-script-loc default-directory)))
                      ))))
             ))) ;; end "standard run" cases
          )
    (if perlnow-trace
        (message "   Returning from 'guess'"))
    (if perlnow-trace (perlnow-close-func))
    run-string))

(defun perlnow-cpan-style-test-run-string (staging-area)
  "Given STAGING-AREA, return appropriate run-string for tests."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-cpan-style-test-run-string"))
  (let* (run-string)
    (setq staging-area (shell-quote-argument staging-area))
    (cond ((file-exists-p (concat staging-area "Build.PL"))
           (setq run-string (concat "cd " staging-area "; ./Build build && ./Build test"))
           )
          ((file-exists-p (concat staging-area "Makefile.PL"))
           (setq run-string (concat "cd " staging-area "; make test"))
           ))
    (if perlnow-trace (perlnow-close-func))
    run-string))

(defun perlnow-test-run-string-harder (harder-setting)
  "Generate a run-string for a through non-cpan-style test run.
Uses the HARDER-SETTING \(4 or 16\) to choose whether to do a
\"prove\" or \"prove -r\"."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-test-run-string-harder"))
  (let ( run-string  t-dir )
    (setq t-dir (car (perlnow-find-t-directories)))
    ;; if no t directory was found, try to create according to default policy.
    ;;    TODO test this behavior, new as of 2017
    (cond ((not t-dir)
           (setq t-dir (perlnow-scan-tree-for-t-loc))
           (perlnow-ensure-directory-exists t-dir)))
    (setq run-string
          (cond ( (>= harder-setting 16) ;; even harder!
                  (concat "cd " t-dir "; prove --nocolor -r")) ;; TODO --nocolor may be out-of-date now
                ( t
                  (concat "cd " t-dir "; prove --nocolor *.t"))
                ))
    (if perlnow-trace (perlnow-close-func))
    run-string))


;;;========
;;; calling external processes

(defun perlnow-shell-command (program &rest args)
  "Call PROGRAM using \\[shell-command] with any given arguments.
Appends output to buffer named `perlnow-message-buffer-name'.
Internally uses \\[shell-command], but runs \\[shell-quote-argument] on all ARGS."
  (let ( initial-buffer  buffer-pair  log-buffer  temp-buffer  cmd
         program-messages  datestamp  message-label )
    (setq initial-buffer (current-buffer))
    (setq buffer-pair (perlnow-get-display-buffers))
    ;; (setq log-buffer  (nth 0 buffer-pair))
    (setq temp-buffer (nth 1 buffer-pair))
    (perlnow-blank-out-display-buffer temp-buffer)  ;; TODO does this *need* to be done here?
    (setq message-label
          (concat
           (format
            "%s run with:\n  %s" program (pp-to-string args))))
    ;; Using call-process would leave you without your envar settings (e.g. to PATH)
    (setq cmd (concat program " " (mapconcat 'shell-quote-argument args " ")))
    (shell-command cmd temp-buffer temp-buffer)
    ;; extract messaging from temp-buffer, and append to the real log-buffer
    (perlnow-move-temp-messages-to-log message-label) ;; does a datestamp first, btw
    (switch-to-buffer initial-buffer)
    ))

;; (perlnow-shell-command "perl" "Build.PL")

;;;========
;;; The *perlnow* log buffer system

;; The goal here is to be able to display output from shell commands
;; without ending up with a display buffer in the user's face, but also
;; without forcibly closing all windows except the one or two I care about
;;
;; The only thing I've found that works is to use an indirect temp buffer,
;; copy the messaging over to the hidden log buffer, then do a kill-buffer on the temp.
;;

(defun perlnow-get-display-buffers ()
  "Returns a pair of buffer objects, a persistant log and a temp buffer.
Uses globals to store buffers: `perlnow-message-buffer', `perlnow-temp-buffer'.
Only recreates them if needed.  The log buffer is named using
`perlnow-message-buffer-name' and the temp buffer is named with \"-temp\"
appended to that name."
  (let ( initial-buffer log-buffer  temp-buffer-name  temp-buffer buffer-pair )
    (setq initial-buffer (current-buffer))
    (setq log-buffer
          (cond ((buffer-live-p perlnow-message-buffer)
                 perlnow-message-buffer)
                (perlnow-message-buffer-name
                 (setq perlnow-message-buffer
                       (get-buffer-create perlnow-message-buffer-name)))
                (t
                 (message
                  (concat
                   "WARNING: perlnow is running without a log buffer, "
                   "because perlnow-message-buffer-name is nil")))
                ))
    (setq temp-buffer-name
          (cond (perlnow-message-buffer-name
                 (let ((name (replace-regexp-in-string "\\*$" "" perlnow-message-buffer-name)))
                   (setq name (concat name "-temp*"))
                   name)
                 )))
    (setq temp-buffer
          (cond ((buffer-live-p perlnow-temp-buffer)
                 perlnow-temp-buffer)
                (temp-buffer-name
                 (setq perlnow-temp-buffer
                       (get-buffer-create temp-buffer-name)))
                ))
    (setq buffer-pair (list log-buffer temp-buffer))
    buffer-pair))
;; (perlnow-get-display-buffers)

(defun perlnow-move-temp-messages-to-log ( &optional message-label )
  "Extract messaging from temp-buffer, and append to the real log-buffer."
  (unless message-label (setq message-label ""))
  (let ( initial-buffer  buffer-pair  log-buffer  temp-buffer
         program-messages  datestamp  )
    (setq datestamp (format-time-string "%a  %B %d, %Y  %R"))
    (setq buffer-pair (perlnow-get-display-buffers))
    (setq log-buffer  (nth 0 buffer-pair))
    (setq temp-buffer (nth 1 buffer-pair))
    (set-buffer temp-buffer)
    (setq program-messages (buffer-string))
    (set-buffer log-buffer)
    (goto-char (point-max))
    (insert datestamp)
    (insert "\n")
    (insert message-label)
    (insert program-messages)
    (insert "\n")
    ;; Note: this does a kill-buffer, which is the *entire* point
    ;; of this exercise, I don't *want* to leave a temp buffer open
    ;; displaying inane messages, I want them tucked away in my log buffer.
    (perlnow-delete-buffer-window-safeish temp-buffer)
    ;; For some reason, that *had* to be done after the above *inserts*,
    ;; otherwise they would  try to go elsewhere (??)
    ))


;;;========
;;; window/buffer management

(defun perlnow-delete-buffer-window-safeish (buffer-or-name)
  "Goal: remove BUFFER-OR-NAME from current display safeishly.
As written the safeishness needs improvement:
I suspect that if the buffer was the only thing displayed in a
frame, it would remove the frame, and I'm not sure I want that
behavior."
  ;; TODO do I need to handle the "but that was the only thing here case"
  ;; Maybe: an optional backing-buffer to display if there's nothing
  ;; else that makes sense?
  (let* ((initial-buffer (current-buffer)))
    (switch-to-buffer buffer-or-name)
    (kill-buffer)
    (switch-to-buffer initial-buffer)
    ))

;; unused
(defun perlnow-hide-buffer ( &optional buffname )
  "Get rid of buffer display window, defaults to the perlnow log *perlnow*."
  (unless buffname (setq buffname perlnow-message-buffer-name))
  (let ((initial-buffer (current-buffer)))
    (switch-to-buffer buffname)
    ;;   (require 'frame-cmds)
    ;;   (old-delete-window (selected-window)) ;; plain delete-window can delete frame
    (if (not (one-window-p t))
        (delete-window (selected-window)))
    (bury-buffer perlnow-message-buffer-name)
    (switch-to-buffer initial-buffer)))


;;; When I want to show a new window alongside the existing
;;; current window, I often close all others and just display the
;;; two of them.
(defun perlnow-new-window-below (&optional numblines)
  "Like split-window-vertically, but handles window size issues without errors.
Optional NUMBLINES requests that many lines in the new, lower, window.
If there aren't at least twice as many lines available, the NUMBLINES request
is ignored, and we fall back on splitting into two equally sized windows."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-new-window-below"))
  (let* ((lines-available (perlnow-window-total-height))
         )
  (cond ((and numblines (>= lines-available (* 2 numblines)))
         (split-window-vertically (* -1 numblines)))
        (t
          (split-window-vertically)
          ))
  (if perlnow-trace (perlnow-close-func))
  ))

(defun perlnow-window-total-height ()
  "Wrapper around \\[window-total-height], returns 40 if it's not found.
A backwards compatibility issue: want to degrade gracefully rather than
throw an error if the window-total-height function isn't available.
Note: when this is called in noninteractive mode \(when running
emacs with --script\), returns 30 \(I was seeing window-total-height return
9 before\)."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-window-total-height"))
  (let ( height )
    (setq height
          (cond ((functionp 'window-total-height)
                 (window-total-height))
                (noninteractive
                 30)
                (t
                 40)))
    (if perlnow-trace (perlnow-close-func))
    height))

(defun perlnow-open-file-other-window (file &optional numblines template stay-here)
  "Open FILE in another window, leaving the current buffer visible.
Options: NUMBLINES, the number of lines in the new
window (defaults to half of frame height); TEMPLATE a
template.el template to be used in creating a new file
buffer.  If STAY-HERE is true, the cursor is left in the
original window, not the new one.
Note: as written the given FILE is expect to have the full path."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-open-file-other-window"))
  (if perlnow-debug
      (message
       (format "\"open other\" w/ %s" (pp-to-string (list file numblines template stay-here)))))
  (unless perlnow-perl-sub-name
    (setq perlnow-perl-sub-name ""))
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
    (if stay-here
        (other-window 1))
    (if perlnow-trace (perlnow-close-func))
    ))

;; Used by perlnow-test-file-menu, perlnow-select-file
;; perlnow-milla, perlnow-module-starter, perlnow-h2xs
;; perlnow-display-inc-array,  perlnow-report-status-vars
(defun perlnow-show-buffer-other-window (buffer &optional numblines stay-here)
  "Utility to open BUFFER in another window, leaving current
visible.  Options: NUMBLINES, the number number of lines in
the new window, defaults to half window height;
If STAY-HERE is true, the cursor is left in the original window,
not the new one. BUFFER can be a string or a buffer object."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-show-buffer-other-window"))
  (unless numblines
    (setq numblines (/ (frame-height) 2) )) ; new window defaults to half of frame height
  (delete-other-windows)
  ;; (split-window-vertically numblines) ; Number of lines to display
  (perlnow-new-window-below numblines)
  (other-window 1)

  (switch-to-buffer buffer) ;; not set-buffer, so buffer is visible
  (if stay-here
      (other-window 1))
  (if perlnow-trace (perlnow-close-func)) )

;; Used by:
;; perlnow-milla, perlnow-module-starter, perlnow-h2xs
;; perlnow-shell-command, perlnow-display-inc-array,  perlnow-report-status-vars
(defun perlnow-blank-out-display-buffer (buffer &optional stay-here)
  "Clear out a temporary display BUFFER.
Erase the contents of a buffer, though only if it matches
the convention for temporary display buffers, i.e. it has
a name beginning with an asterix.  Create it if it doesn't exist.
Returns the buffer object.  Argument BUFFER can be a string or
a buffer object.  This can work on a read-only buffer."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-blank-out-display-buffer"))
  (let (;; (original-buff (buffer-name))
        (original-default-directory default-directory)
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
    (if stay-here
        (switch-to-buffer buffer))
    (if perlnow-trace (perlnow-close-func))
    (setq default-directory original-default-directory)
    ))

;; Used by perlnow-milla
(defun perlnow-dual-window-display (top-fob bot-fob &optional top-height bot-height)
  "In current frame, display only the two given file-or-buffers (aka \"fob\"s).
TOP-FOB will be shown on top, and BOT-FOB on the bottom.
The number of screen lines may be specified with TOP-HEIGHT
or BOT-HEIGHT... if both are specified, TOP-HEIGHT will be used.
Something very, very sensible is done if the number of lines
requested is impossible or impractical.
See: \\[perlnow-new-window-below],  because I don't remember what it does.
Leaves the cursos in the top window."
  (cond (top-height
         (setq bot-height (- (perlnow-window-total-height) top-height)))
        ((not bot-height)
         (setq bot-height perlnow-secondary-window-size)
         ))
  (delete-other-windows)
  (cond((bufferp top-fob)
        (switch-to-buffer top-fob) )
       (t
        (find-file top-fob)))
  (perlnow-new-window-below bot-height)
  (other-window 1)
  (cond((bufferp bot-fob)
        (switch-to-buffer bot-fob) )
       (t
        (find-file bot-fob)))
  (other-window 1) )

;;=======
;; the "do" routines: internal, lower-level routines used by the external "entry point" functions

(defun perlnow-do-script (filename)
  "Quickly jump into development of a new perl script.
Prompts the user for the FILENAME.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script]."
  (interactive
   (perlnow-prompt-user-for-file-to-create
    "Name for the new perl script? " perlnow-script-location))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-do-script"))
  (require 'template)
  (perlnow-create-with-template filename (perlnow-choose-script-template))
  (perlnow-change-mode-to-executable)
  (if perlnow-trace (perlnow-close-func))
  )

(defun perlnow-do-script-from-module (script package &optional incspot)
  "Does the work of creating a script from a module-buffer.
Takes arguments SCRIPT, PACKAGE, and INC-SPOT,
which are all explained in `perlnow-documentation-terminology'.
If INC-SPOT is nil, it skips adding the FindBin/use lib lines.
It's expected that the user will not usually run this directly.
See the wrapper function: \\[perlnow-script] (or possibly the older
\\[perlnow-script-using-this-module])."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-do-script-from-module"))
  ;; Presumption: if incspot is nil, then we got here from a man page buffer,
  ;; and we can assume the module is installed (or the man page most
  ;; likely wouldn't be there).
  (let* ((initial (current-buffer))
         (man-page-p (eq incspot nil))
         (created))
    (unless man-page-p
      (perlnow-sub-name-to-var))

    ;; module is displayed, now want to open script, show in paralel
    (perlnow-open-file-other-window script nil (perlnow-choose-script-template))
    (setq created (current-buffer))

    ;; Make the script we've created the default run-string for this module.
    (set-buffer initial)
    (setq perlnow-run-string
          (cond (man-page-p
                 (perlnow-generate-run-string script))
                (t
                 (perlnow-generate-run-string-and-associate script))
                ))
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
            (insert (format "use %s%s; # added by perlnow" package import-string))
            (insert "\n")
            )))
    (perlnow-change-mode-to-executable)
    (if perlnow-trace (perlnow-close-func))
    script))

(defun perlnow-endow-script-with-access-to (location &optional whitespace)
  "Insert appropriate \"use lib\" line so script will see given LOCATION."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-endow-script-with-access-to"))
  (cond ((not (and perlnow-suppress-use-lib-if-in-inc
                   (not (perlnow-incspot-in-INC-p location))))
         (let* ((script-name (buffer-file-name))
                (relative-path
                 (file-relative-name location (file-name-directory script-name))))
           (unless whitespace (setq whitespace "")) ;; Default to empty string
           (insert (format "%suse FindBin qw\($Bin\);\n" whitespace))
           (insert (format "%suse lib \(\"$Bin/" whitespace))
           (insert relative-path)
           (insert "\");\n"))
         ))
  (if perlnow-trace (perlnow-close-func))
  )

;; Used by:
;;   perlnow-open-test-file
;;   perlnow-do-script-from-module

(defun perlnow-import-string-from (package-name &optional incspot-opt)
  "Get a workable import string from the module PACKAGE-NAME.
If the module is not exporter-based, this is just the empty string,
if it is, it will be the contents of the EXPORT and EXPORT_OK arrays.
If the module is installed, this can work just with the
PACKAGE-NAME, otherwise, the optional INC-SPOT-OPT is needed to point
at it."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-import-string-from"))
  (if perlnow-debug
      (message (format "\"import-string\" w/ %s %s" package-name (pp-to-string incspot))))
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
    (if perlnow-trace (perlnow-close-func))
    import-string))

;; ========
;; efficient (?) set subtraction (using a hash table)
;; (used by perlnow-revise-export-list)

(defun perlnow-string= (a b)
  "Essentially just another string=.
You may call it redundant, but it makes me feel beter."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-string="))
  (let* ((ret (compare-strings a nil nil b nil nil nil))
         )
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-string-hash (key)
  "Tries (and evidentally, fails) to strip text properties."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-string-hash"))
  (let (( limit (- (length key) 1) ))
    (sxhash
     (progn
       (set-text-properties 0 limit nil key)
       key))
    (if perlnow-trace (perlnow-close-func))
    ))

(define-hash-table-test 'perlnow-test
 'perlnow-string= 'perlnow-string-hash)

(defun perlnow-subtract-lists (list1 list2)
  "Subtracts LIST2 from LIST1.
Returns a list containing values of LIST1 that are not
found in LIST2."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-subtract-lists"))
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
    (if perlnow-trace (perlnow-close-func))
    difference))


;; ========
;; manipulate associated code files together

(defun perlnow-sync-save-run-string (run-string &optional harder-setting)
  "Save RUN-STRING value consistently in the appropriate locations.
Assigns the given string to the run-string of the current
buffer, and also of the \\[perlnow-associated-code] buffer if
that seems appropriate.
Sets either \\[perlnow-run-string] or \\[perlnow-run-string-harder]
depending on the value of the given HARDER-SETTING."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-sync-save-run-string"))
  (perlnow-save-run-string-hard-aware run-string harder-setting)
  ;; TODO don't munge the asscode run string unless it makes sense
  ;;      EXPERIMENTAL: skip it if both buffers are scripts.
  (let* ((this-file (buffer-file-name))
         (assoc-file perlnow-associated-code))
    (cond ((not (and
                 (perlnow-script-file-p this-file)
                 (perlnow-script-file-p assoc-file)))
           (let* ((initial (current-buffer)))
             (perlnow-back-to-code)
             (perlnow-save-run-string-hard-aware run-string harder-setting)
             (switch-to-buffer initial))))
    (if perlnow-trace (perlnow-close-func))
    run-string))

(defun perlnow-sync-save-run-string-OLD (run-string &optional harder-setting)
  "Save RUN-STRING value consistently in the appropriate locations.
Assigns the given string to the run-string of the current
buffer, and also of the \\[perlnow-associated-code] buffer.
Sets either \\[perlnow-run-string] or \\[perlnow-run-string-harder]
depending on the value of the given HARDER-SETTING."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-sync-save-run-string"))
  (perlnow-save-run-string-hard-aware run-string harder-setting)
  (let* ((initial (current-buffer)))
    ;; (save-excursion ;; trust this not, to restore the original buffer (?)
    (perlnow-back-to-code)
    (perlnow-save-run-string-hard-aware run-string harder-setting)
    ;; )
    (switch-to-buffer initial)
    (if perlnow-trace (perlnow-close-func))
    run-string))

(defun perlnow-save-run-string-hard-aware (run-string &optional harder-setting)
  "Save the RUN-STRING for the current buffer for the given HARDER-SETTING."
  ;; (if perlnow-trace (perlnow-open-func "Calling " "perlnow-save-run-string-hard-aware"))
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
  ;; (interactive);; DEBUG
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-follow-associations-to-non-test-code"))
  (let* ((initial (current-buffer))
         (here (or filename (buffer-file-name)))
         (limit 100)
         (count 0)
         target
         )
    ;; TODO intermittant stringp from test file menu: trying to narrow it down.
    (if perlnow-debug (message "A"))
    (unless here
      (error "Current buffer must be a file buffer, or you need to specify a filename."))
    (save-excursion
      (if here
          (if perlnow-debug (message "B"))
          (find-file here))
      (cond (perlnow-associated-code
             (if perlnow-debug (message "C"))
             (let ((next perlnow-associated-code))
               (find-file next)
               (catch 'UP
                 (if perlnow-debug (message "D"))
                 (while (not (perlnow-code-but-not-test-p))
                   (progn
                     (if perlnow-debug (message "E"))
                     (if (> (setq count (1+ count)) limit)
                         (throw 'UP nil))
                     (setq next perlnow-associated-code)
                     (if perlnow-debug (message "F"))
                     (find-file next)
                     )))
               (setq target next)))
            (t
             (if perlnow-debug (message "G"))
             (setq target here)
             )
            ))
    (if perlnow-debug (message "H"))
    (switch-to-buffer initial) ;; save-excursion doesn't always work
    (if perlnow-debug (message "I"))
    (if perlnow-trace (perlnow-close-func))
    target))

(defun perlnow-set-associated-code-pointers (there &optional here)
  "Make THERE the associated code for HERE (default: current buffer's file).
Revises the buffer-local variable \\[perlnow-associated-code] in
both locations. Note: expects both HERE and THERE to be existing files.
Opens the files, if not open already. Both arguments should be full paths."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-set-associated-code-pointers"))
  (let* ((initial (current-buffer))
         (here    (or here (buffer-file-name)))
         retval )
    (cond (perlnow-debug
           (message "Associating here and there...")
           (message (format "here: %s"  (pp-to-string here)))
           (message (format "there: %s" (pp-to-string there)))))
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
                     "'THERE' not even a string, needs to be a full file path"))
                   (message "HERE is: %s" here) )
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
                     "'HERE' not even a string, needs to be a full file path"))
                   (message "THERE is: %s" there))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-generate-run-string-and-associate (program-file)
  "Generates a direct run-string for the perl PROGRAM-FILE.
This is used internally by routines such as \\[perlnow-guess-run-string].
This version is a convienience routine which also associates the
given PROGRAM-FILE with the current buffer's file."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-generate-run-string-and-associate"))
  (let* ( (runstring "") )
    (setq runstring
          (perlnow-generate-run-string program-file))
    (perlnow-set-associated-code-pointers program-file)
    (if perlnow-trace (perlnow-close-func))
    runstring))

;; TODO verify that program-file is code? (perlnow-perl-code-p file)
(defun perlnow-generate-run-string (program-file)
  "Generates a direct run-string for the perl PROGRAM-FILE.
This is used internally by routines such as \\[perlnow-guess-run-string].
If PROGRAM-FILE is nil or empty-string, this returns an empty-string."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-generate-run-string"))
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
    (if perlnow-trace (perlnow-close-func))
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
 (if perlnow-trace (perlnow-open-func "Calling " "perlnow-associate-last-with-this"))
 (let* ((there perlnow-last-buffer-file-name)
        (here (buffer-file-name))
        retval-if-any
        )
   (setq retval-if-any
         (cond ((and
                 (perlnow-perl-code-p there)
                 (perlnow-perl-code-p here))
                (perlnow-set-associated-code-pointers there here)
                (if perlnow-debug
                    (message
                     "perlnow-associate-last-with-this:\nhere: %s\n there: %s\n" here there))
                )))
   (if perlnow-trace (perlnow-close-func))
   retval-if-any))

;; (remove-hook 'next-error-hook 'perlnow-associate-last-with-this) ;; DEBUG
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-prompt-for-module-to-create"))
  (let* ((filename (perlnow-full-path-to-module where what))
         (dirname (convert-standard-filename (file-name-directory filename))))
    (while (file-exists-p filename)
      (setq what
            (read-from-minibuffer "That module name is already in use. Please choose another: " what))
      (setq filename (perlnow-full-path-to-module where what)))
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-prompt-for-cpan-style"))
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
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-prompt-for-cpan-style-again"))
  (if perlnow-trace (perlnow-close-func))
  (list where what))

(defun perlnow-prompt-user-for-file-to-create (ask-mess default-location)
  "Ask for the name of the file to create.
Check to see if one exists already, and if so, ask for another name.
Asks the question ASK-MESS, and defaults to the using the location
DEFAULT-LOCATION.  Returns a list of a single string, full file name
with path."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-prompt-user-for-file-to-create"))
  (let ( filename )
    (setq default-location (file-name-as-directory default-location))
    (while (progn
             (setq filename
                   (expand-file-name
                    (read-file-name ask-mess default-location)))
             (setq ask-mess
                   "That name is already in use, please use another name: " )
             (file-exists-p filename)))
    (if perlnow-trace (perlnow-close-func))
    (list filename)
    ))
;; end  prompt functions

;;========
;; file creation

(defun perlnow-make-sure-file-exists ()
  "Forcibly save the current buffer to it's associated file.
This is to make sure that the file actually exists."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-make-sure-file-exists"))
  (set-buffer-modified-p t)
  (save-buffer)
  (if perlnow-trace (perlnow-close-func)))

(defun perlnow-change-mode-to-executable ()
  "Make the file associated with the current buffer executable."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-change-mode-to-executable"))
  (perlnow-make-sure-file-exists)
  (let* (
;;         (all-but-execute-mask ?\666)
         (all-but-execute-mask #o666)  ;; March 28, 2017
         (filename (buffer-file-name))
         (file-permissions (file-modes filename))
         (new-file-permissions
          (+ (logand file-permissions all-but-execute-mask)
             perlnow-executable-setting)
          ))
    (set-file-modes filename new-file-permissions)
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-create-with-template (filename template &optional force)
  "Create and open a new file with a template.el template.
Given FILENAME and TEMPLATE this does the actual creation of the
file and associated buffer using the template.  As a side-effect,
it sets the global `template-file' here.  Returns t on
success (i.e. checks that file exists).  If either the FORCE
option or the `perlnow-quiet' variable is set, creates any needed
directories silently, and moves existing files out of the way,
renaming with an '.OLD' suffix."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-create-with-template"))
  (if perlnow-debug
      (message (format "\"create w template\"\n  %s\n  %s" filename template )))
  ;; override local 'force' setting with global (this silences questions)
  (if perlnow-quiet (setq force perlnow-quiet))
  (unless perlnow-perl-sub-name (setq perlnow-perl-sub-name ""))
  (let* ((backup-name (concat filename ".OLD"))
         (loc (file-name-directory filename))
         ret  )
    (cond (force
           (cond ((file-exists-p filename)
                  (rename-file filename backup-name t)))
           (cond ((not (file-directory-p loc))
                   (make-directory loc t)))
           ))
    ;; The "template-file" must be set here because of a bug in
    ;; template.el, when using template-new-file non-interactively.
    (setq template-file (template-split-filename filename))
    (template-new-file filename template)
    (write-file filename)
    (if (perlnow-perl-code-p filename)
        (funcall (perlnow-lookup-preferred-perl-mode)))
    (setq ret
          (file-exists-p filename)) ;; return t on success
    (if perlnow-trace (perlnow-close-func))
    ret))

;;--------
;; choose template

(defun perlnow-choose-module-template ( &optional cpan-style module-style template-location )
  "Return an appropriate template to use to create a module.
Uses CPAN-STYLE and MODULE-STYLE if they're provided, but if no
template exists to match them, silently falls back to the closest
match.  Looks for templates in TEMPLATE-LOCATION,  but defaults
to the value of `perlnow-template-location'."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-choose-module-template"))
  (let* ((file-suffix "pm")
         (template
          (perlnow-choose-template file-suffix cpan-style module-style template-location )))
    (if perlnow-trace (perlnow-close-func))
    template))

;; "pm-t"
;; last "t"
(defun perlnow-choose-module-t-template ( &optional cpan-style module-style template-location )
  "Return an appropriate template to use to create a test for a module.
Uses TEMPLATE-TAG and MODULE-STYLE if they're provided, but if no
template exists to match them, silently falls back to the closest
match.  Looks for templates in TEMPLATE-LOCATION,  but defaults
to the value of `perlnow-template-location'."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-choose-module-template"))
  (let* ((file-suffix "pm-t")
         (template
          (perlnow-choose-template file-suffix cpan-style module-style template-location )))
    (if perlnow-trace (perlnow-close-func))
    template))

;; TEMPLATE.perlnow-milla-<huh?>-pl.tpl
;; TEMPLATE.perlnow-milla-pl.tpl
;; TEMPLATE.perlnow-pl.tpl
;; TEMPLATE.pl.tpl
;; "pl"
(defun perlnow-choose-script-template ( &optional cpan-style module-style template-location )
  "Return an appropriate template to use to a perl script.
Uses TEMPLATE-TAG if provide \(and if you're so inclined, MODULE-STYLE,
though that makes little sense for a script\), but if no template
exists to match them, silently falls back to the closest match.
Looks for templates in TEMPLATE-LOCATION, but defaults to the
value of `perlnow-template-location'."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-choose-module-template"))
  (let* ((file-suffix "pl")
         (template
          (perlnow-choose-template file-suffix cpan-style module-style template-location )))
    (if perlnow-trace (perlnow-close-func))
    template))

(defun perlnow-choose-script-t-template ( &optional cpan-style module-style template-location )
  "Return an appropriate template to use create a test for a perl script.
Uses TEMPLATE-TAG if provide \(and if you're so inclined, MODULE-STYLE,
though that makes little sense for a script\), but if no template
exists to match them, silently falls back to the closest match.
Looks for templates in TEMPLATE-LOCATION, but defaults to the
value of `perlnow-template-location'."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-choose-module-template"))
  (let* ((file-suffix "pl-t")
         (template
          (perlnow-choose-template file-suffix cpan-style module-style template-location )))
    (if perlnow-trace (perlnow-close-func))
    template))

;; core functionality for above choose template routines
(defun perlnow-choose-template ( file-suffix &optional cpan-style module-style template-location )
  "Return an appropriate template to use to create a perl file.
This is a routine internally used by \\[perlnow-choose-module-template],
\\[perlnow-choose-module-t-template], \\[perlnow-choose-script-template]
and so on.
The FILE-SUFFIX is typically a string like \"pm\", \"pl\", or \"pm-t\".
Uses TEMPLATE-TAG and MODULE-STYLE if they're provided, but if no
template exists to match them, silently falls back to the closest
match.  Looks for templates in TEMPLATE-LOCATION,  but defaults
to the value of `perlnow-template-location'."
  ;; We do a two-stage "last ditch" attempt at a match where if there's
  ;; a hyphen in file-suffix, in tries one last time with a trimmed
  ;; down suffix, just the part after the hyphen. this covers a snag
  ;; where the final thing tried sometimes deviates from the pattern,
  ;; e.g. pm-t last-ditches to just t
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-choose-template"))
  (unless template-location (setq template-location perlnow-template-location))
  (let* ( candidate template )
    (setq template
          (catch 'FIT
            (cond ((setq candidate (perlnow-template-override file-suffix cpan-style module-style))
;;                   (if (file-exists-p candidate) (throw 'FIT candidate))))
                   (if candidate (throw 'FIT candidate))))
            (cond ((and cpan-style module-style)
                   (setq candidate
                         (format
                          "%sTEMPLATE.perlnow-%s-%s-%s.tpl"
                          template-location
                          cpan-style
                          module-style
                          file-suffix
                          ))
                   (if (file-exists-p candidate) (throw 'FIT candidate))))
            (cond (module-style
                   (setq candidate
                         (format
                          "%sTEMPLATE.perlnow-%s-%s.tpl"
                          template-location
                          module-style
                          file-suffix
                          ))
                   (if (file-exists-p candidate) (throw 'FIT candidate))
                   ))
            (setq candidate
                  (format
                   "%sTEMPLATE.perlnow-%s.tpl"
                   template-location
                   file-suffix
                   ))
            (if (file-exists-p candidate) (throw 'FIT candidate))
            (setq candidate
                  (format
                   "%sTEMPLATE.%s.tpl"
                   template-location
                   file-suffix
                   ))
            (if (file-exists-p candidate) (throw 'FIT candidate))
            ;; if there's a hyphen try just the portion after it
            (cond ((string-match "-\\(.*\\)$" file-suffix)
                   (setq file-suffix (match-string 1 file-suffix))
                   (setq candidate
                         (format
                          "%sTEMPLATE.%s.tpl"
                          template-location
                          file-suffix
                          ))
                   (if (file-exists-p candidate) (throw 'FIT candidate))
                   ))
            ))
    (unless template
      (message "perlnow-choose-module-template could not find a template in:\n %s" template-location)
      template-location)
    (if perlnow-trace (perlnow-close-func))
    template))

(defun perlnow-template-override (file-suffix cpan-style module-style)
  "Uses the FILE-SUFFIX and MODULE-STYLE to lookup a template override var.
The older template vars are now used as overrides, to keep perlnow from
searching for an appropriate template on it's own:
 `perlnow-perl-module-template'
 `perlnow-perl-object-module-template'
 `perlnow-perl-test-module-template'
 `perlnow-perl-script-template'
 `perlnow-perl-test-script-template'.
Note: CPAN-STYLE is actually unused here, it's a place-holder to maintain
a consistent in-family interface."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-template-override"))
  (let (template)
    (setq template
          (cond ((and (equal module-style "object") (equal file-suffix "pm"))
                  perlnow-perl-object-module-template)
                 ((equal file-suffix "pm")
                  perlnow-perl-module-template)
                 ((equal file-suffix "pm-t")
                  perlnow-perl-test-module-template)
                 ((equal file-suffix "pl")
                  perlnow-perl-script-template)
                 ((equal file-suffix "pl-t")
                  perlnow-perl-test-script-template)
                  ))
    (if perlnow-trace (perlnow-close-func))
    template))


;; end file creation

;;========
;; recently used perl buffers from buffer-list

;; TODO NEXT 
;; new project: BUFFLISTFILT    Thu  March 08, 2018  18:44  tango

;; Example code 
;; (defun perlnow-file-open-p (full-file-name)
;;   "Given a FULL-FILE-NAME, return t if there's an open buffer for it.
;; Otherwise, return nil.  The given full file name should be an absolute path."
;;   (let ((open-p nil))
;;     (dolist (bfn (mapcar 'buffer-file-name (buffer-list)))
;;       (if (equal full-file-name bfn)
;;           (setq open-p t)))
;;     open-p))


;; Various ideas: go through buffer-list, extract the perlish buffers. (( Done ))


;; find the next module; find the next test; find the next script or test.

;; find the next *whatever* associated with the current project (same tree as a given file).

;; TODO make sure this includes a test select buffer?  Or exclude it on purpose?
(defun perlnow-perl-buffers ()
  "Return a list of open perl buffers in most recently used order.
Here a perl buffer is one with a mode name that looks perlish."
  (let (pat  perl-buffer-list)
    (setq pat "\\b[Cc]*[Pp]erl\\b")  
     (dolist (bf (buffer-list))
       (set-buffer bf)
       (let* (;; (fn (buffer-file-name bf))
              (mn mode-name) )
         (if (string-match pat mn)
             (push bf perl-buffer-list))  
         ))
     (reverse perl-buffer-list)))

;; (message (pp (perlnow-perl-buffers)))

;;; TODO getting there, but this is in inverse order:
;;; (#<buffer mapgame> #<buffer read_new> #<buffer last_kdvs> #<buffer BoxFind.pm> #<buffer thumbnailer> #<buffer webwrap> #<buffer image_rescale> #<buffer junkgen_all_daily.pl> #<buffer show_images_three_lists> #<buffer restamp> #<buffer list_vidarc> #<buffer testes-file_existance_empty_string.pl> #<buffer Rank.pm> #<buffer 06-Draw-WaveStack-hm.t> #<buffer WaveStack.pm> #<buffer hdmi> #<buffer wave_stack.pl>)

;; Bleh "consing on the end" doesn't do what I expect (or course), but a reverse works, so live with that:
;; "(#<buffer wave_stack.pl> #<buffer hdmi> #<buffer WaveStack.pm> #<buffer 06-Draw-WaveStack-hm.t> #<buffer Rank.pm> #<buffer testes-file_existance_empty_string.pl> #<buffer list_vidarc> #<buffer restamp> #<buffer show_images_three_lists> #<buffer junkgen_all_daily.pl> #<buffer image_rescale> #<buffer webwrap> #<buffer thumbnailer> #<buffer BoxFind.pm> #<buffer last_kdvs> #<buffer read_new> #<buffer mapgame>)



;;========
;; buffer/file probes (determine what kind of code it is, etc)
(defun perlnow-nix-script-p ()
  "Determine if the buffer looks like a 'nix style executable script.
Looks for the hash-bang line at the top."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-nix-script-p"))
  (save-excursion
    (let ((hash-bang-line-pat "^[ \t]*#!")
           ret )
      (goto-char (point-min))
      (setq ret
            (looking-at hash-bang-line-pat))
      (if perlnow-trace (perlnow-close-func))
      ret)))

(defun perlnow-script-p (&optional file)
  "Determine if the buffer looks like a perl script.
If FILE is given, opens that first.
This assumes we have a perl script if there's a
perl hashbang line *or* if it is in a perl mode,
and also verifies that it's not a module."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-p"))
  (save-restriction
    (widen)
    (let ((initial-buffer (current-buffer))
          retval  )
      (cond (file
             (find-file file))
            (t
             (setq file (buffer-file-name))))
      (save-excursion
        (let (;; The following assumes perl is called something like perl
              (hash-bang-line-pat "^[ \t]*#!.*perl") )
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
                  )))))
      (switch-to-buffer initial-buffer)
      (if perlnow-trace (perlnow-close-func))
      retval)))

(defun perlnow-script-not-test-p (&optional file)
  "Similar to \\[perlnow-script-p], but excludes perl test code."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-not-test-p"))
  (let* ((retval  nil)
         (initial-buffer (current-buffer))
         (initial-point  (point))
         ret  )
    (save-excursion
      (if file (find-file file))
      (setq ret (and (perlnow-script-p) (not (perlnow-test-p)))) )
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-perl-mode-p (&optional file)
  "Does the current buffer's major mode look like a perl mode?
This will return non-nil for \"sepia-mode\" as well as \"perl-mode\"
and \"cperl-mode\", but not for \"perlnow-select-mode\".
If FILE is given, opens that first."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perl-mode-p"))
  (let ((initial-buffer (current-buffer)) )
    (cond (file
           (find-file file))
          (t
           (setq file (buffer-file-name))))
    (let* (;; matches cperl-mode, perl-mode, or sepia-mode, and *not* perlnow-select-mode
           (perl-mode-pat "^\\\(sepia\\\|cperl\\\|perl\\\)-mode$")
           (mode (pp-to-string major-mode))
           (retval (string-match perl-mode-pat mode))
           )
      (switch-to-buffer initial-buffer) ;; save-excursion doesn't always work
      (if perlnow-trace (perlnow-close-func))
      retval)))

(defun perlnow-test-p (&optional file)
  "Determine if FILE looks like a perl test, defaulting to the current buffer.
Looks for a *.t extension on file name, then looks for a 'use Test::' line."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-p"))
  (save-restriction
    (widen)
    (let* ((use-test-pattern "\\b\\(require\\|use\\)[ \t]+Test::\\b")
           (initial-buffer (current-buffer))
           (initial-point  (point))
           retval )
      (save-excursion
        (cond((not file)
              (setq file (buffer-file-name)))
             (t
              (find-file file)))
        (cond ((and file (string-match "\\\.t$" file))
               (goto-char (point-min))
               (if (re-search-forward use-test-pattern nil t)
                   (setq retval t))
               )) )
      (switch-to-buffer initial-buffer)
      (goto-char initial-point)
      (if perlnow-trace (perlnow-close-func))
      retval)))

(defun perlnow-module-code-p (&optional file)
  "Determine if the buffer looks like a perl module.
If given FILE, opens that first.  This looks for the package line
near the top, and checks for a file extension of \"pm\".
Note: it's often more useful to just try to get the package
name directly: \\[perlnow-get-package-name-from-module]."
  (interactive);; DEBUG
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-code-p"))
  (let* ((initial-buffer (current-buffer))
         ext  package-name )
      (cond (file (find-file file) )
            (t
             (setq file (buffer-file-name))
             ))
      (if file
          (setq ext (file-name-extension file)))
      (if (string= ext "pm")
          (setq package-name (perlnow-get-package-name-from-module)))
      (switch-to-buffer initial-buffer)
      ;; (message "%s" package-name);; DEBUG
      (if perlnow-trace (perlnow-close-func))
      package-name))

(defun perlnow-exporter-code-p (&optional file)
  "Return t if the current buffer looks like an Exporter-based module.
Return nil otherwise."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-exporter-code-p"))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-cpan-style-code-p ()
  "Determine if this file looks like it's in a cpan-style dev tree.
Returns the staging-area name, or nil if not found.
From the current file-buffer, climbs upwards, looking for a level
with a Makefile.PL or a Build.PL."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-cpan-style-code-p"))
  ;; as written, this also checks for a "lib" or "t" dir next to it.
  (let* ((staging-area (perlnow-find-cpan-style-staging-area)))
    (if perlnow-trace (perlnow-close-func))
    staging-area))


;; TODO MAYBE
;; I might want to change how this works: if loc/default-directory is
;; a point in an odd location in a milla project rather than
;;   <project_root>/lib, then we should maybe pretend it's not
;; a milla project, so the module creation commands will still work.
;; in the unusual place.
(defun perlnow-milla-p ( &optional loc )
  "Determine if given location is in a milla \(or dzil\) cpan-style project.
Checks LOC option or `default-directory', if not given.
At present, just looks for the location of a dist.ini file in the staging-area."
  (unless loc (setq loc default-directory))
  (let ( staging-area  milla-p  )
    (setq staging-area (perlnow-find-cpan-style-staging-area loc))
    (cond ((and staging-area
                (perlnow-find-location-with-target staging-area (list "dist.ini") "^d" ))
           (setq milla-p t)))
    milla-p))

;; (perlnow-milla-p "/home/doom/tmp/devosity/Plink-A-Dink/lib") ;; t
;; (perlnow-milla-p "/home/doom/tmp/devosity/Plink-A-Dink/") ;; t
;; (perlnow-milla-p "/home/doom/tmp/devosity/Plink-A-Dink")  ;; nil  ;;;; TODO badzoidal
;; (perlnow-milla-p "/home/doom/tmp/devosity/lib")   ;; nil
;; (perlnow-milla-p "/home/doom/tmp/devosity/lib/")  ;; nil
;; (perlnow-milla-p "/home/doom/tmp/devosity/")      ;; nil
;; (perlnow-milla-p "/home/doom/tmp/devosity")       ;; nil

(defun perlnow-perl-code-p (&optional file)
  "Return t if FILE seems to be perl code, defaults to current-buffer.
Checks for the usual perl file extensions, and if need
be opens the file to look for a package line or a hashbang line."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perl-code-p"))
  (let ((initial (current-buffer))
        retval  )
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-code-but-not-test-p (&optional here)
  "Is HERE (default: current buffer) a perl file that is not a test?"
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-code-but-not-test-p"))
  (let* ((initial (current-buffer))
         test-file-p  here  retval
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-module-file-p (&optional file)
  "Determine if the FILE looks like a perl module."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-file-p"))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-script-file-p (&optional file)
  "Determine if the FILE looks like a perl script."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-file-p"))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-test-select-menu-p ()
  "Identify whether the current buffer looks like a test select menu.
Checks mode and buffer name."
;; TODO could also look for lines with text property perlnow-file-path
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-test-select-menu-p"))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-dired-buffer-p ()
  "Return nil if not a dired buffer, and non-nil if it is.
Actually, this just passes through the return from \\[perlnow-dired-buffer-dir],
which is probably what you're always going to want to use."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-dired-buffer-p"))
  (let ((dir (perlnow-dired-buffer-dir)))
    (if perlnow-trace (perlnow-close-func))
    dir))

(defun perlnow-dired-buffer-dir ()
  "If this is a dired buffer, return the dir it eds.
Returns nil if not a dired buffer. If there's more than one
associated location, just returns the first of them."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-dired-buffer-dir"))
  (let ( location )
    ;; if dired buffer use that to get location
    (cond ((equal (symbol-name major-mode) "dired-mode")
           (cond ((listp dired-directory)
                  (setq location (car dired-directory)))
                 (t
                  (setq location dired-directory))))
          (t
           (setq location nil)))
    (if perlnow-trace (perlnow-close-func))
    location))

;; Used by: perlnow-list-perl-tests, perlnow-stepup-for-pm-loc, perlnow-scan-tree-for-t-loc
(defun perlnow-current-context ()
  "Returns the file or directory for the active buffer."
  (let* ((context (or (buffer-file-name)
                      (perlnow-dired-buffer-dir)
                      (perlnow-select-read-full-file-name) ;; nil if not select test buffer
                      default-directory)))
    context))

;; end buffer probes

;;========
;; buffer scraping -- get metadata about the code buffer

;; An example of typical code to get and unpack metadata
;; using the following routine:

;; (defun bloorg ()
;;   ""
;;   (let* ((md (perlnow-metadata))
;;          (testloc-absolute (nth 0  md))
;;          (hyphenized       (nth 1  md))
;;          (package-name     (nth 2  md))
;;          (incspot          (nth 3  md))
;;          (buffer           (nth 4  md))
;;          (filename         (nth 5  md))
;;          (fileloc          (nth 6  md))
;;          (basename         (nth 7  md))
;;          (file-type        (nth 8  md))
;;          (project-type     (nth 9  md))
;;          (sub-name         (nth 10 md))

;; Used by: perlnow-test-create and perlnow-test-create-manually.
;;
;; TODONT The test code creation problem is nastier than you
;; think (always, even for me, even now) and you will *not* replace
;; this code with something way simpler, so stop thinking about it.
(defun perlnow-metadata (&optional file-name)
  "Tries to give you \"metadata\" for the relevant perl code.
Based on your present context (as inferred from the current buffer,
of the FILE-NAME if given), it determines various pieces of
information often needed by different perlnow functions.

If a module file is open, it tells information about that module,
if a test file is open, it tells you about the test file, but
also tries to determine the module being tested by the code, and
provides some information about that.  The case of a script
file is similar to that of the test file \(though at present,
not as well supported\).

If a \"*select test file*\" buffer, it again tries to tell you
something about the module it figures you're testing, making
inferences based on the current line.

The metadata is returned as an ordered list, where the first
three elements are currently unused ((used to be the tp trio))

An example of returned metadata.

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-metadata"))
  (save-restriction
    (widen)
    (let ((initial-point  (point))
          (initial-buffer (current-buffer))
          md-list
          testloc-absolute
          package-name  incspot  hyphenized-package-name
          buffer
          file-name  file-location  basename
          file-type  project-type
          sub-name  staging-area   script-name  test-name

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
      (if perlnow-debug
          (message "perlnow-metadata: file-type: %s project-type: %s from file-name: %s "
                   file-type project-type (pp-to-string file-name)))

      ;; package-name set already for module and object and maybe man-page
      ;; TODO better to do the others here?  Make a guess for test and script

      ;; going after package-name (mostly)
      (cond
       ((string= file-type "test-select-menu")
        ;; get the module name from the test file name
        (let* ((selected-file-compact (perlnow-current-line-stripped))
               (path (perlnow-get-path-from-markedup-name selected-file-compact))
               (testfile (concat path selected-file-compact)))
          (setq package-name (perlnow-module-from-t-file testfile t))
          (setq testloc-absolute (perlnow-t-dir-from-t testfile))

          ;; (perlnow-incspot-from-t testfile) ;; TODO is this result thrown away?
          (setq incspot (perlnow-incspot-from-t testfile ))

          (setq file-name (perlnow-full-path-to-module incspot package-name))
          (setq file-location file-name)
          (setq basename (file-name-sans-extension (file-name-nondirectory file-name)))

          ;; the select menu buffer, which is literally the current buffer.  TODO okay?
          (setq buffer    (current-buffer))  ;; TODO factor out above.  there's always a buffer.
          )) ;; end test-select-menu
       (;; Man or WoMan buffer
        (setq package-name (perlnow-get-package-name-from-man))
        (setq file-type "man-page"))
       (t ;; not a test-select-menu or man page: assuming it's a basic file-buffer
        ;; We do all of the following for every file buffer
        (cond ((setq file-name     (buffer-file-name))
               (setq file-location (file-name-directory file-name))
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
          (setq package-name (perlnow-get-package-name-from-module))
          (setq sub-name (or (perlnow-sub-at-point) ""))
          (setq testloc-absolute (perlnow-scan-tree-for-t-loc))
          (perlnow-ensure-directory-exists testloc-absolute)

          (if perlnow-debug
              (message "perlnow-metadata: package-name: %s file-location: %s" package-name file-location))
          (if perlnow-debug (message "perlnow-metadata: testloc-absolute: %s "   testloc-absolute))
          (setq incspot (perlnow-get-incspot package-name file-location))
          )
         ((string= file-type "test")
          (let* ((path (file-name-directory file-name))
                 (testfile file-name)
                 (fields (perlnow-parse-standard-t-name testfile))
                 (prefix      (nth 0 fields))
                 (hyphenized  (nth 1 fields))
                 (subname     (nth 2 fields))
                 (description (nth 3 fields))
                 (colonized (replace-regexp-in-string "-" "::" hyphenized))
                 )
            (setq package-name colonized)
            (setq testloc-absolute (perlnow-t-dir-from-t testfile))
            (setq incspot (perlnow-incspot-from-t testfile ))
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
          (setq testloc-absolute (perlnow-scan-tree-for-t-loc))
          (perlnow-ensure-directory-exists testloc-absolute)

          (cond (perlnow-associated-code
                 (let* ( (candidate (perlnow-follow-associations-to-non-test-code))
                         (ext (file-name-extension candidate))
                         )
                   (unless ext (setq ext ""))
                   (cond ((string-match "pm$" ext)
                          (setq package-name
                                (perlnow-get-package-name-from-module candidate))
                          (setq incspot (perlnow-get-incspot package-name candidate))
                          )))))
          (unless package-name
            ;; TODO move/copy this sort of thing to a context='cpan' block?
            (cond ((setq staging-area (perlnow-find-cpan-style-staging-area))
                   (setq incspot staging-area)
                   (let* ((incspot-trimmed
                           (perlnow-remove-trailing-slash incspot))
                          (incspot-path      (file-name-directory    incspot-trimmed))
                          (incspot-sans-path (file-name-nondirectory incspot-trimmed)))
                     (setq hyphenized-package-name incspot-sans-path)
                     (setq package-name (mapconcat 'identity (split-string hyphenized-package-name "-") "::"))
                     )) ;; end cpan-style
                  (t ;; script not in cpan-style, and no package-name defined yet
                   (setq package-name (perlnow-package-from-use-line))
                   )
                  ))
          ))) ;; ;; end file-type script and end basic-file buffer
       ) ;; end going after package-name
      (cond ((and package-name (not hyphenized-package-name))
             (setq hyphenized-package-name
                   (mapconcat 'identity (split-string package-name "::") "-"))
             ))
      ;; originally, this was a very important side-effect... doing it here isn't so logical, though
      (if (and testloc-absolute incspot)
          (perlnow-stash-put testloc-absolute incspot))

      (setq md-list
            (list
             testloc-absolute                      ;;  USEFUL for case 2
             hyphenized-package-name               ;;  USEFUL for case 1
             package-name
             incspot                               ;;  USEFUL for case 1 & 2
             buffer file-name file-location basename
             file-type
             project-type
             sub-name
             ))
      (if perlnow-debug (perlnow-report-metadata md-list))
      ;; returning from any excursions
      (switch-to-buffer initial-buffer)
      (goto-char initial-point)
      (if perlnow-trace (perlnow-close-func))
      md-list)))


(defun perlnow-connect-t-loc-and-lib (&optional testloc-absolute incspot)
  "Stash the association between TESTLOC-ABSOLUTE and INCSPOT."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-connect-t-loc-and-lib"))
  (let ( md )
    (cond ((not (and testloc-absolute incspot))
           (setq md (perlnow-metadata))
           (setq testloc-absolute (nth 0  md))
           (setq incspot          (nth 3  md))
           ))
    (if (and testloc-absolute incspot)
        (perlnow-stash-put testloc-absolute incspot))
    (if perlnow-trace (perlnow-close-func))
    md))
  ;; might be a useful return, e.g. for perlnow-test-file-menu


;;======
;; code info extraction and navigation

(defun perlnow-hashbang ()
  "What is the hash bang line for this file buffer?
Returns nil if there is none."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-hashbang"))
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
      (if perlnow-trace (perlnow-close-func))
      return)))

(defun perlnow-sub-at-point ()
 "Returns the name of the current perl sub, or nil if there is none.
When run inside an open buffer of perl code.  It tries to find
the name of the current perl sub \(the one that the cursor is
either inside of, or just in front of\).  Returns nil on failure,
sub name on success."
  (interactive) ;; DEBUG
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-sub-at-point"))
  (let* ((sub-meta (perlnow-range-current-sub))
         (sub-name (nth 0 sub-meta))
         )
  (if perlnow-trace (perlnow-close-func))
  sub-name))

;; Used by perlnow-script (via perlnow-do-script-from-module)
;; perlnow-edit-test-file (via perlnow-open-test-file)
(defun perlnow-sub-name-to-var ()
  "Assigns the current perl sub name to `perlnow-perl-sub-name'.
It is then available as the \(>>>PERL_SUB_NAME<<<\) template expansion.
This is intended to be run inside an open buffer of perl code.
The \"current sub\" is as determined by \\[perlnow-sub-at-point].
Returns nil on failure, sub name on success."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-sub-name-to-var"))
  (let ((sub-name (or (perlnow-sub-at-point) "")))
    (setq perlnow-perl-sub-name sub-name)
    (if perlnow-trace (perlnow-close-func))
    perlnow-perl-sub-name))

(defun perlnow-range-current-sub ()
  "Gets perl sub metadata.
Returns three values in a list: subname, begin and end.
Subname should be empty if we're above the first sub in the file.
Blocks of pod and comments above a sub count as part of it."
  (interactive) ;; DEBUG
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-range-current-sub"))
  (let* ((initial-point (point)) ;; 0
         ;; (open-curly-pat "[\\{]")          ;; open curly brace (TODO optimize?)
         ;; (sub-begin-pat "^[ \t]*sub ")     ;; perl "sub" keyword at bol
         )
    (let ( sub-meta-list-prev sub-meta-next subname  return
           ret-subname beg  ret-beg  end ret-end  ret-list )
      (save-excursion
        (setq ret-list
              (catch 'IT
                ;; get info from the previous start-of-sub onwardsx
                (cond ((perlnow-move-back-to-sub-keyword)  ;; puts you before "sub "
                       (setq sub-meta-list-prev (perlnow-sub-after-point))
                       (cond (sub-meta-list-prev
                              (let* ((subname  (nth 0 sub-meta-list-prev))
                                     (beg      (nth 1 sub-meta-list-prev))
                                     (end      (nth 2 sub-meta-list-prev)))
                                ;; if pt in range, we're inside that sub
                                (cond ((and (>= initial-point beg)
                                            (<= initial-point end))
                                       (throw 'IT sub-meta-list-prev)))
                                ;; get ready to search for next, get to end of prev sub
                                (goto-char end)))
                             (t
                              (message "perlnow-range-current-sub: weirdzo case")
                              )))
                      (t ;; there is no prev sub, so back to where we started
                       (goto-char initial-point)))
                ;; Tue  November 20, 2018  19:31  implementing a no-subs-land above first sub
                (setq sub-meta-list-next (perlnow-sub-after-point))
                (cond (sub-meta-list-next
                       (let* ((subname  (nth 0 sub-meta-list-next))
                              (beg      (nth 1 sub-meta-list-next))
                              (end      (nth 2 sub-meta-list-next)))
                         ;; if initial-point is up above range of first sub, then there's no current sub.
                         (if (< initial-point beg)  
                             (throw 'IT (list "" 0 (point))))
                         )
                       (throw 'IT sub-meta-list-next)
                       ))
                ))
        (message "perlnow-range-current-sub: %s" (pp-to-string ret-list))
        (if perlnow-trace (perlnow-close-func))
        ret-list))))

;; Used in: perlnow-range-current-sub
(defun perlnow-move-back-to-sub-keyword ()
  "Move point to just before the sub keyword of the current sub.
If there is no preceeding sub keyword, this performs no
operation, and returns nil.  This does move up even when
down below the closing brace of a sub.  Skips anonymous subs.
Returns point to indicate success.
The main intented use is to prepare for a search back to the
previous sub by first ensuring you're outside of the present one."
  (interactive) ;; DEBUG
  (let* ((no-properties t)
         (sub-begin-pat "^[ \t]*sub ")     ;; perl "sub" keyword at bol, ws allowed
         )
    (let ( ret )
      (cond (
             (and
              (string= "sub" (thing-at-point 'word no-properties))
              (not (perlnow-in-pod-p)))
             (unless (string= "sub " (buffer-substring (point) (+ (point) 4)))
               (backward-word 1))
             (setq ret (point)))
            ((re-search-backward sub-begin-pat nil t 1)  ;; puts you before "sub "
             ;; but if that leading "sub " is in a pod block, keep looking
             (while (perlnow-in-pod-p)
               (re-search-backward sub-begin-pat nil t 1))
             (setq ret (point)))
            (t
             (setq ret nil)))
      ;; now that we've found a "sub" keyword, check if it's an anonymous sub
      (cond (ret
             (while (perlnow-sub-with-no-name-p)
               (cond ((re-search-backward sub-begin-pat nil t 1)  ;; puts you before "sub "
                      (setq ret (point)))
                     (t
                      (setq ret nil)))
               )
             ))
      ret)))


(defun perlnow-sub-with-no-name-p ()
  "If this is an anonymous sub, return t.
Expects to be run with the point at the beginning of the \"sub\" keyword."
  (interactive) ;; DEBUG
  (let* ((initial-point (point))
         ;; (open-curly-pat "[\\{]")          ;; open curly brace
         (after-subname-pat "[\\\\(\\{:]") ;; paren, curly or colon
         )
    (let ( beg  end  gap-str  no-name )
      ;; presumes we're at the start of a sub keyword
      ;; TODO would like to enforce this, and skip past a pod sub
      ;; skip to end of keyword: save location
      (forward-word 1) ;; now at end of "sub" keyword
      (setq beg (point))
      ;; search forward to brace, save location
      (re-search-forward after-subname-pat nil t)  ;; just after curly or paren
      (backward-char 1) ;; now, just on the brace
      (setq end (point))

      ;; check substring
      (setq gap-str (buffer-substring-no-properties beg end))

      ;; is there a name there, or is it nothing but \s?
      (cond ((string-match "^\s*$" gap-str)
             (setq no-name t)
             )
            (t
             (setq no-name nil)
             ))
      (message "no-name: %s" (pp-to-string no-name))
      (goto-char initial-point)
      no-name)))

(defun perlnow-sub-after-point ()
  "Looks for the next sub definition and determines the name and the extent.
Returns a list of: name, begining, ending, and start of sub keyword.
The range of the sub includes any block of pod preceeding the sub,
and the ending is the location of the closing curly brace.
If started up *on* the sub keyword will skip to next sub."
  ;; Do all the comments here disturb you?  Such are the wages of parsing via regexps
  (interactive) ;; DEBUG
  (let* ((initial-pt (point))
         (sub-begin-pat "^[ \t]*sub ")     ;; perl "sub" keyword at bol
         ;; (after-subname-pat "[ \\\\(\\{]") ;; either paren or curly, after space
         ;; (after-subname-pat " [\\\\(\\{]") ;; either paren or curly, after space
         (after-subname-pat "[\\\\(\\{:]") ;; paren, curly or colon
         (open-curly-pat "[\\{]")          ;; open curly brace
         (pod-pat "^=")                    ;; any pod-tag
         (pod-cut-pat "^=cut")             ;; =cut, the one exit from pod
         )
    (let ( ret-list  beg-name  end-name  subname  beg-sub-keyword
                     beg-sub  end-sub  gap-str  gap-not-empty )
      (cond
            ;;; TODO
            ;;;      a block here  to handle the case of starting on a "sub" keyword
       (
             (re-search-forward sub-begin-pat nil t 1)  ;; puts you after "sub "
             (backward-word 1)         ;; at start of "sub"

             (setq beg-sub-keyword (point)) ;; provisional start-of-range
             (re-search-forward sub-begin-pat nil t 1)  ;; puts you after "sub "
             (forward-word 1)
             (backward-word 1)  ;; puts you at start of subname
             (setq beg-name (point))
             (re-search-forward after-subname-pat nil t)  ;; just after curly or paren
             (backward-word 1)
             (forward-word 1)  ;; puts you at end of subname
             (setq end-name (point))
             (setq subname
                   (buffer-substring-no-properties beg-name end-name))
             (re-search-forward after-subname-pat nil t)  ;; just *after* curly or paren
             (backward-char 1)

             ;; if there's no *curly* at point, search for one
             (unless (string= (buffer-substring-no-properties (point) (1+ (point))) "{")
               (re-search-forward open-curly-pat nil t)
               )  ;; now we're on the curly
             (forward-sexp 1) ;; on to the closing brace
             (setq end-sub (point))
             (goto-char beg-sub-keyword) ;; return to the start of sub keyword
             (re-search-backward pod-cut-pat nil t) ;; at start of "=cut"
             (forward-word 1)  ;; at end of "=cut"
             (setq gap-str
                   (buffer-substring-no-properties (point) beg-sub-keyword))
             ;; ensure that the gap between the found pod and the sub is
             ;; composed of blank lines (possibly with commented lines)
             (setq gap-not-empty nil)
             (dolist (line (split-string gap-str "[\n]"))
               (unless
                   (or
                   (string-match "^\s*$" line)
                   (string-match "^\s*#" line))
                 (setq gap-not-empty t)))
             (cond ((not gap-not-empty) ;; gap is blank: pod preceeds the sub
                    ;; search back to the pod tag that preceeds the end-of-pod
                    (beginning-of-line 1)   ;; now at start of "=cut"
                    (re-search-backward pod-pat nil t)  ;; drops off at start of "=item"
                    (beginning-of-line 1)               ;; redundancy is goooood
                    (setq beg-sub (point)) ;; now the range should include the pod
                    )
                   (t
                    (setq beg-sub beg-sub-keyword) ;; presume no pod preceeding sub
                    ))
             (setq ret-list
                   (list subname beg-sub end-sub beg-sub-keyword))
             (if perlnow-debug
                 (message "perlnow-sub-after-point: %s" (pp-to-string ret-list)))
             ))
      (goto-char initial-pt) ;; preserves initial cursor location.
      ret-list)))

;; Used in perlnow-move-back-to-sub-keyword
(defun perlnow-in-pod-p ()
  "Returns t if point is in a pod block.
Works by checking for the in-pod text-property."
  (interactive)
  (let* ((chk (get-text-property (point) 'in-pod)) )
;;     (if perlnow-debug
;;         (message "in-pod: %s" (pp-to-string chk)))
    chk))

;;;--------
;;; more sub info: all subs in buffer
;;   (also see perlnow-sub-at-point above)

;; Used by:  perlnow-revise-export-list
(defun perlnow-list-all-subs ( &optional internals )
  "Extracts the sub names for all routines in the current buffer.
Presumes the current buffer is a perl module.  If the INTERNALS
option is set to t, (TODO) subs with leading underscores are included,
otherwise they're skipped."
  (interactive) ;; DEBUG only
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-list-all-subs"))
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
           sub-list  )
      (while
          (re-search-forward sub-pattern nil t) ;; uses current buffer
        (progn
          (setq sub-name (match-string-no-properties 1))
          (if (or internals (not (string-match "^_" sub-name)))
                 (setq sub-list (cons sub-name sub-list)))
          ))
      (if perlnow-trace (perlnow-close-func))
      sub-list)))

;; TODO FOR REAL better: insert-other-window
;;               maybe:  grab-from-other-window
(defun perlnow-all-subs-report ()
  "Dump listing of all subs in current buffer."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-all-subs-report"))
  (let* (( sub-list (perlnow-list-all-subs))
         )
    (if perlnow-debug
        (message "%s" sub-list))
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-report-list-all-subs ()
  "Reports a list of all subs to *Messages*/stderr."
  (interactive)
  (message "all-subs: %s"
           (pp-to-string
            (perlnow-list-all-subs))))

;-----
;; code navigation

;; Used only by: perlnow-open-test-file
(defun perlnow-jump-to-use (package-name &optional import-string)
  "Given the PACKAGE-NAME, jumps to the point before the \'use\' line.
Specifically, these leaves the cursor at the start of the line
that does a \"use\" or \"use_ok\" of the named module specified in
perl's double-colon seperated form, e.g. \"Modular::Stuff\".
If given the optional IMPORT-STRING, incorporates it into the use line."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-jump-to-use"))
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
    (if perlnow-trace (perlnow-close-func))
    whitespace))

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-list-all-exported-symbols"))
  (unless (perlnow-module-code-p)
    (error "perlnow-list-all-exported-symbols expects to be called from a module buffer."))
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
    (if perlnow-trace (perlnow-close-func))
    export-list))

(defun perlnow-list-all-exported-symbols-report ()
  "Echoes output of \\[perlnow-list-all-exported-symbols] via message."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-list-all-exported-symbols-report"))
  (let* ( (list (perlnow-list-all-exported-symbols) )
          )
    (message "%s" list)
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-package-name"))
  (save-restriction
    (widen)
    (let (return)
      (cond
       ((setq return (perlnow-get-package-name-from-module))
        )
       ((setq return (perlnow-get-package-name-from-man))
        )
       (t
        (setq return nil)
        ))
      (if perlnow-trace (perlnow-close-func))
      return)))

(defun perlnow-get-package-name-from-module (&optional file-name)
  "Get the module name from the first package line.
This will be in perl's double colon separated form, or it will
return nil if none is found.  If a FILE-NAME is given, will open
that file, otherwise tries to work on the current buffer.
Note: this is often used to determine if the current buffer
looks like a perl module."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-package-name-from-module"))
  (save-restriction
    (widen)
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
        (if perlnow-trace (perlnow-close-func))
        module-name))))

(defun perlnow-get-package-name-from-man ()
  "Return the module name from a man page buffer for perl documentation.
Otherwise, returns nil."
  ;; (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-package-name-from-man"))
  (let ( return  candidate
         m0 m1 m2 m3
         )
    (save-excursion
      (goto-char (point-min))
      ;; scrape first line to ensure we're looking at a perl module man page
      (let ((top-line (perlnow-current-line))
            (buffer-name-string (buffer-name))
            (perl-doc-1-pat "User Contributed Perl Documentation")
            (perl-doc-2-pat "Perl Programmers Reference Guide")
            (man-section-three-pat "(3)")
            (module-name-extractor-pat " \\([^ ]*?\\)\\*")
            )
        ;; (message "top-line: %s" top-line)
        (cond ((and (or
                     (string-match perl-doc-1-pat top-line)
                     (string-match perl-doc-2-pat top-line))
                    (string-match man-section-three-pat top-line))
               (cond ((or
                       (string= major-mode "woman-mode")
                       (string= major-mode "Man-mode"))
                      (cond (buffer-name-string
                             (cond
                              ((string-match module-name-extractor-pat buffer-name-string)
                               (setq m1 (match-string 1 buffer-name-string))
                               ;; (message "found mod name: %s" m1)
                               ))))
                      ))
               ))
        (setq candidate m1)
        (if perlnow-trace (perlnow-close-func))
        candidate))))

;; simple variant of package-name scraping, to get the incspot (done frequently)
(defun perlnow-incspot-if-module ()
  "If the current buffer is a perl module, return the incspot.
Otherwise, return nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-incspot-if-module"))
  (let (package-name file-name file-location lib-loc incspot)
    (setq incspot
          (cond ((setq package-name (perlnow-get-package-name-from-module))
                 (setq file-name     (buffer-file-name))
                 (setq file-location (file-name-directory file-name))
                 (perlnow-get-incspot package-name file-location) )
                (t nil)))
    (if perlnow-trace (perlnow-close-func))
    incspot))


;;;---------
;;; perlnow-module-from-t-file (package name scraping continued)

;; Used by perlnow-metadata, perlnow-select-create-test
(defun perlnow-module-from-t-file ( &optional t-file colonized-flag )
  "Try to infer an associated module given a test file, T-FILE.
If not provided as an argument, the T-FILE will be read from
near point, presuming a \"*select test file*\" menu is the
current buffer.  Return defaults to \"hyphenized\" form, but
the COLONIZED-FLAG option can be set to request double-colon separators."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-from-t-file"))
  (cond ((not t-file)
         (let*
             ((selected-file-compact (perlnow-current-line-stripped))
              (path (perlnow-get-path-from-markedup-name selected-file-compact))
              (t-file (concat path selected-file-compact))
              )))) ;; end cond not t-file

  (let ( colonized  hyphenized  fields  ret )
    (setq fields (perlnow-parse-standard-t-name t-file))
    (if perlnow-debug
        (message "perlnow-module-from-t-file, fields: \n%s" (pp-to-string fields)))
    (cond ((setq hyphenized (nth 1 fields))
           (setq colonized (perlnow-colonize hyphenized)))
          ;; No hyphenized field, and looks like a script test, e.g. "03-glowing-script.t"
          ((equal (nth 3 fields) "script") ;; TODO any better check with a regexp against t-file?
           (setq colonized  (perlnow-package-from-script-t t-file))
           (setq hyphenized (perlnow-hyphenize colonized))
           )
          ;; try to get module from a use_ok line in the *.t
          ((setq colonized (perlnow-get-package-name-from-useok t-file))
           (setq hyphenized (replace-regexp-in-string "::" "-" colonized))
           )
          ;; TODO try still other methods of scraping module info?
          (t
           (message "Could not determine module from t-file.")
           ))
    (setq ret (cond
               (colonized-flag colonized)
               (t hyphenized)))
    (if perlnow-trace (perlnow-close-func))
    ret))

;; yet another bland name that sounds like all of the others
;; Used by: perlnow-module-from-t-file
(defun perlnow-package-from-script-t ( &optional script-t-file )
  "Determine the package name associated with a *.t file of a perl script.
Defaults to examining the current-buffer, but optionally looks at SCRIPT-T-FILE.
Uses \\[perlnow-script-to-be-tested-by-script-t] internally."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-package-from-script-t"))
  (save-excursion
    (unless script-t-file (setq script-t-file (buffer-file-name)))
    (unless (string-match "-script\\.t$" script-t-file)
      (message
       (concat "WARNING: perlnow-package-from-script-t only works "
               (format "on a perlnow-style script test file, this name looks wrong: %s"
                       script-t-file))))
    (let ( perl-script-t-file  colonized )
    (setq perl-script-t-file (perlnow-script-to-be-tested-by-script-t script-t-file))
    (cond ((file-exists-p perl-script-t-file)
           (find-file perl-script-t-file)
           ;; scrape a module from a script buffer
           (setq colonized (perlnow-package-from-use-line)))
          (t
           (message
            "WARNING: perlnow-package-from-script-t can't work: does not exist: %s "
            script-to-file)
           (setq colonized nil)))
    (if perlnow-trace (perlnow-close-func))
    colonized)))

;; Only used by perlnow-package-from-script-t
(defun perlnow-script-to-be-tested-by-script-t ( &optional script-t-file )
  "Extract the file name of the script to be tested by the given SCRIPT-T-FILE.
Defaults to the current-buffer.
Note: this code relies fairly tightly on perlnow conventions such as:
  o  script tests are named \"*-script.t\"
  o  the full path to the script to be tested is assigned to $script_name  "
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-to-be-tested-by-script-t"))
  (save-excursion
    (unless script-t-file (setq script-t-file (buffer-file-name)))
    (unless (string-match "-script\\.t$" script-t-file)
      (message
       (concat "WARNING: perlnow-script-to-be-tested-by-script-t only works "
               (format "on a perlnow-style script test file, this name looks wrong: %s"
                       script-t-file))))
    (find-file script-t-file)
    (goto-char (point-min))
    (let (  open-quote-pat  close-quote-pat  pat1  p1  p2  perl-script-file  )
      ;; Looking for this line:
      ;; my $script_name = '/home/doom/End/Cave/Perlnow/lib/perlnow/t/dat/code/s65/non1/bin/glowing.pl';
      (setq open-quote-pat  "['\"]")
      (setq close-quote-pat "['\"]")
      (setq pat1 (concat "my\s+\\$script_name\s*=\s*" close-quote-pat))
      (ignore-errors
        (setq p1 (re-search-forward  pat1))
                 (re-search-forward  close-quote-pat)
        (setq p2 (re-search-backward close-quote-pat))
        (setq perl-script-file (buffer-substring p1 p2)))
      (unless perl-script-file
        (message
         "WARNING: %s could not determine which perl script is tested by: %s"
         "perlnow-script-to-be-tested-by-script-t"
         script-t-file))
      (if perlnow-debug
          (message "perl-script-file: %s" perl-script-file))
      (if perlnow-trace (perlnow-close-func))
      perl-script-file)))

;; Used by: perlnow-module-from-t-file
(defun perlnow-get-package-name-from-useok ( &optional t-file )
  "Get the module package name found in a use_ok line.
A perl test file often has a uses \"use_ok\" rather than \"use\",
and that can be a good hint about which module the test file
is primarily intended to exercise.  Example:

    use_ok( 'WildSide::Glam' ,  qw(sashay) );

When there's more than one use_ok line in the file, this picks the last one.
By default presumes the current buffer shows a *.t file, but a T-FILE to examine
can be passed  in as an optional argument."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-package-name-from-useok"))
  (let* ((initial-buffer (current-buffer))
         (initial-point  (point))
         (useok "use_ok")
         ;; Note: this should skip any commented out 'use_ok' lines
         (find-pat (concat "^\\s*?" useok "\\b"))
         (capture-pat (perlnow-package-capture-from-useok-pat))
         useok-line   quoted-package   package-name  cmd   )
    (save-excursion
      (if t-file
          (find-file t-file))
      (save-restriction
        (widen)
        (goto-char (point-max))
        (re-search-backward find-pat nil t)
        (setq useok-line (perlnow-current-line))
        ;; trim string to the quoted package name with capture-pat
        (string-match capture-pat useok-line)
        ;; get the package-name with surrounding perl quotes
        (setq quoted-package
              (match-string 1 useok-line))
        ;; you might think you use perl to interpret it's quoting, but then
        ;; you've got shell argument quoting issues, so...
        (setq package-name
              (perlnow-strip-perl-quotage quoted-package))
        ;; covering for save-excursion flakiness
        (switch-to-buffer initial-buffer)
        (goto-char initial-point)
        ))
    (if perlnow-trace (perlnow-close-func))
    package-name))

(defun perlnow-package-capture-from-useok-pat ()
  "Generate a regexp to extract a package-name with quotes from a use_ok line.
This regexp punts on the problem of interpreting the myriad types
of allowed perl quotes and just includes them with the package-name."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-package-capture-from-useok-pat"))
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
    (if perlnow-trace (perlnow-close-func))
    capture-pat))

;; Used by perlnow-get-package-name-from-useok
(defun perlnow-strip-perl-quotage (str)
  "Remove perl-style quoting from the given STR and return it.
Perl quotes include various qq{} variants as well as singles and doubles."
;; Now, with extra-added ugly hackiness.
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-strip-perl-quotage"))
  (let* ((beg-pat-ng "^[ \t]*?\\(?:['\"]\\|q.?[\\W]?\\)[ \t]*?")
         (end-pat-ng "[ \t]*?['\"\\W]*[ \t]*?$")
         (pat1 "^[ \t'\"]")
         (pat2 "[ \t'\"]$")
         (pat3 "^qq?[{\(\[\|^#% ]+")
         (pat4 "[}\)\]\|^#% ]+$")
         (pat5 "[}|\)]$") )
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
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-parse-standard-t-name"))
  (let* ((t-file-basename
          (file-name-sans-extension (file-name-nondirectory t-file)))
         (fragments (split-string t-file-basename "-"))
         (integer-pat     "^[0-9]+$" ))
    (let ( fragment  words  prefix  hyphenized  subname  description )
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
            (mapconcat 'identity (reverse words) "-"))  ;; nreverse
      (if (string= hyphenized "")
          (setq hyphenized nil))
      ;; any remaining fragments are the description
      (if fragments
          (setq description
                (mapconcat 'identity fragments "-")))
      (if perlnow-trace (perlnow-close-func))
      (list prefix hyphenized subname description)
      )))

;; Just used by perlnow-parse-standard-t-name
(defun perlnow-bigletter-p (str)
  "Try to determine whether the single-character string STR is a \"big letter\".
Which is to say, is it capitalized or title-case and not lowercase?
This just looks at the first character of STR, and silently ignores the rest."
  ;; This is a weird algorithm, but it beats trying to figure out what
  ;; get-char-code-property does.  (Maybe the regexp class [:upper:] would work?)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-bigletter-p"))
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
           (if perlnow-trace (perlnow-close-func))
           bigletter-p))))

;; TODO handle a t-dir argument as well as a testfile argument
;; Used by: perlnow-open-test-file and also perlnow-metadata
(defun perlnow-incspot-from-t (testfile)
  "Given TESTFILE return the associated incspot."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-incspot-from-t"))
  (if perlnow-debug
      (message (format "perlnow-incspot-from-t, with testfile: %s" (pp-to-string testfile))))
  (let* (;; in lieu of save-excursion
         (initial-buffer (current-buffer))
         (initial-point  (point))
         (t-loc (perlnow-t-dir-from-t testfile))
         incspot     staging   package-name  start-loc)
    ;; make the testfile buffer open and active
    (find-file testfile)
    (cond ((setq staging (perlnow-find-cpan-style-staging-area))
           (setq incspot (concat staging perlnow-slash "lib")))
          ((setq incspot
                 (perlnow-stash-lookup (file-name-directory testfile))))
          (t
           (setq package-name
                 (perlnow-module-from-t-file testfile t))
           (setq incspot (perlnow-stepup-for-pm-loc package-name))
           ))
    ;; return from any excursions
    (switch-to-buffer initial-buffer)
    (goto-char initial-point)
    (if perlnow-trace (perlnow-close-func))
    incspot))

;; Used by: perlnow-incspot-from-t, perlnow-metadata
;;  and indirectly: perlnow-open-test-file
(defun perlnow-t-dir-from-t ( testfile )
  "Given a TESTFILE with absolute path, looks above it find a \"t\" directory.
Returns path to \"t\" (including \"t\")."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-t-dir-from-t"))
  (let* ((path testfile) ;; starting here handles the no-intermediaries case better
         path-sans-slash  dir  t-loc )
    (setq t-loc
          (catch 'ROCK
            (while (> (length path) 1 ) ;; TODO unix-only?
              (setq path (perlnow-fixdir (concat path "..")))
              ;; TODO build-up this regexp using perlnow-slash
              (setq path-sans-slash (replace-regexp-in-string "/$" "" path))
              (setq dir (file-name-nondirectory path-sans-slash))
              (if (string= dir "t")
                  (throw 'ROCK path)))) )
    (if (< (length t-loc) 1)
        (setq t-loc nil))
    (if perlnow-trace (perlnow-close-func))
    t-loc))

;; end of perlnow-module-from-t-file related functions

;;========
;; functions to navigate project file structure

;; Used by:
;;  perlnow-stepup-for-pm-loc, perlnow-stepup-project-root-via-subdir-names.
;;  Also: perlnow-dirs-from-one-up (unused)
(defun perlnow-dirs (location &optional all-opt )
  "A simple directory listing of given LOCATION, limited to sub-directories.
Always skips the special directories \".\" and \"..\".
Skips any other hidden directories \(and some additional ones like RCS\) unless ALL-OPT is t
Restricts the listing to accessible directories, unless ALL-OPT is t.
Returns nil if LOCATION is nil or empty-string.
The returned list is unsorted."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-dirs"))
  (if perlnow-debug
      (message "perlnow-dirs: %s" (pp-to-string location)))
  (let (items  dirs)
    (cond ((or (not location)
               (string-match "^$" location))
           (setq dirs nil))
          (t
           (setq items (directory-files location t "" t))
           (cond (all-opt
                  (dolist (item items)
                    (if (and
                         (file-directory-p item)
                         (not (string-match "/\\.$"    item)) ;; note: every name preceeded by "/"
                         (not (string-match "/\\.\\.$" item))
                         )
                        (push item dirs))))
                 (t
                  (dolist (item items)
                    (if (and
                         (file-accessible-directory-p item)
                         (not (string-match "/\\."    item)) ;; skip *any* hidden dir
                         (not (string-match "/RCS$"   item))
                         )
                        (push item dirs)))
                  ))
           ))
     (if perlnow-debug
         (message "perlnow-dirs returning: %s" (pp-to-string dirs)))
     (if perlnow-trace (perlnow-close-func))
    dirs))

;;--------
;; use line scraping

;; Used by perlnow-package-from-script-t, and perlnow-metadata
(defun perlnow-package-from-use-line ()
    "When run on a script file, tries to identify the key use line.
A script, particularly one generated with perlnow, will often
have an important module dependency, e.g. a module developed
concurrently with the script.  This tries to identify this
particular use line in the current buffer.  As written,
this looks for the last use line flagged with an \"added
by perlnow\" comment, or alternately just looks for the last
use line."
    ;; Also see the test code routine: perlnow-get-package-name-from-useok
    (if perlnow-trace (perlnow-open-func "Calling " "perlnow-package-from-use-line"))
    (save-excursion
      (let* (;;save-excursion seems flaky
             (initial-buffer (current-buffer))
             (initial-point  (point))
             (pn-label "# added by perlnow")
             ;; patterns to find lines
             (find-use-pat (concat "^\\s*?" "use" "\\b")) ;; skips commented out ones
             (find-pn-add-pat (concat pn-label "[ \t]*?" "$" ))
             ;; pattern to extract package from a line
             (capture-pat (perlnow-package-capture-from-use-pat))
             use-list  use-line  package-name  pick1 pick2  )
        (save-restriction
          (widen)
          (goto-char (point-max))
          (cond ((re-search-backward find-pn-add-pat nil t)
                 (setq use-line (perlnow-current-line))
                 (string-match capture-pat use-line)
                 (setq pick1 (match-string 1 use-line)) ))
          (goto-char (point-max))
          (cond ((re-search-backward find-use-pat nil t)
                 (setq use-line (perlnow-current-line))
                 (string-match capture-pat use-line)
                 (setq pick2 (match-string 1 use-line)) ))
          (setq package-name (or pick1 pick2))
          ;; return from any excursions
        (switch-to-buffer initial-buffer)
        (goto-char initial-point)
        (if perlnow-trace (perlnow-close-func))
        package-name))))

;; Example use lines to scrape (note: name ends with semicolon or space)
;;   use Data::Dumper;
;;   use Data::BoxFormat::Unicode::CharClasses ':all'; # IsHor IsCross IsDelim
;;
;; used by: perlnow-package-from-use-line
(defun perlnow-package-capture-from-use-pat ()
  "Generate a regexp to extract a package-name from a use line."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-package-capture-from-use-pat"))
  (let* (
         (capture-pat  ;; dodging need for a parser again (just barely)
             (concat
              "^[ \t]*?"
              "use"     ;; KISS for now

;;               ;; TODO maybe also allow "require":
;;               "\\(?:"   ;; non-capturing 'shy group'
;;               "use"
;;               "\\|"     ;; or
;;               "require"
;;               "\\)"     ;; close shy group

              "[ \t]+?" ;; require spaces
              "\\("     ;; open capture
              ".*?"     ;;   anything, up to...
              "\\)"     ;; close capture,

              "\\(?:"   ;; non-capturing 'shy group'
              ";"       ;;  a semicolon
              "\\|"     ;; or...
              "[ \t]+"  ;;  some whitespace
              "\\)"     ;; close shy group
              ))
         )
    (if perlnow-trace (perlnow-close-func))
    capture-pat))




;;=======
;;  path crunching

(defun perlnow-full-path-to-module (incspot package-name)
  "Piece together a INC-SPOT and a PACKAGE-NAME into a full file name.
Given \"/home/doom/lib\" and the perl-style \"Text::Gibberish\" would
yield the file system path: \"/home/doom/lib/Text/Gibberish.pm\"."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-full-path-to-module"))
  (let ( filename  fullpath  )
    (setq filename
          (concat
           (mapconcat 'identity (split-string package-name "::") perlnow-slash)
           ".pm"))
    (setq incspot (file-name-as-directory incspot))
    (if perlnow-trace (perlnow-close-func))
    (setq fullpath (concat incspot filename))
    fullpath))

;; general utility, not perlnow-specific
(defun perlnow-one-up (&optional location)
  "Get an absolute path to the location one above the given LOCATION.
This just acts on the given string, ignores any trailing
delimiter (on unix, slash), and tries to strip off the end of the
string up the previous delimiter.  If that can't be done, returns nil.
So the following corner cases return nil:
    empty string; root location; string without any slashs
Relative paths remain relative."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-one-up"))
  (unless location (setq location ""))
  (let* ((slash perlnow-slash)
         (trailing-slash-pat (concat slash "$"))
         (path-sans-slash (replace-regexp-in-string trailing-slash-pat "" location))
         (one-up (file-name-directory path-sans-slash)))
    (if perlnow-trace (perlnow-close-func))
    one-up))

;; Used by: perlnow-stepup-for-pm-loc, perlnow-stepup-path-to-matching-name
;; (general utility, not perlnow-specific)
(defun perlnow-path-tail (&optional path)
  "Given full PATH, returns the last level of the PATH.
A variant of `file-name-nondirectory' that's insensitive to trailing slashes.
I normally store paths with trailing slashes, but that interacts
badly with `file-name-nondirectory': it always just returns the whole path.
This routine returns just the lowest level of the path, without
the path to it.  When called on a file, returns the file name sans path.
Returns nil if given nil, if given a string without slashes,
returns the given string unchanged."
  ;; (if perlnow-trace (perlnow-open-func "Calling " "perlnow-path-tail"))
  (let (result)
    (cond (path
           (let* (;; remove trailing slash so file-name-directory & nondirectory can work
                  (tail-slash-pat (concat perlnow-slash "$"))
                  (path
                   (replace-regexp-in-string tail-slash-pat "" path))
                  (last-level (file-name-nondirectory path)))
             (setq result last-level)))
          (t
           (setq result nil)))
    ;; (if perlnow-trace (perlnow-close-func))
    result))


;;;--------
;;; internal routines for perlnow-edit-test-file (and relatives)

(defun perlnow-get-test-file-name ()
  "Find test file to edit relevent for the current context.

Begins by trying to find testloc-absolute, then looks for
existing tests in there.

Tries to narrow the set by trying to find matches on the module
name (HYPHENIZED-PACKAGE-NAME) and/or `perlnow-perl-sub-name'.

Ultimately chooses the most recently modified file from the
relevant set, or if there are no tests found, return a file name
to be created.

If called on a acript, looks for names with suffix \"-script.t\".
"
  (if perlnow-trace (perlnow-open-func "Calling perlnow-get-test-file-name"))
  (perlnow-sub-name-to-var)
  (cond ((not perlnow-perl-sub-name)
         (setq perlnow-perl-sub-name ""))) ;; nil causes problems
  (let* ( test-file ;; return value
          test-name-pat
          test-files
          hyphenized-package-name
          file-location
          )
    (setq file-location
          (file-name-directory (buffer-file-name)))
    (setq hyphenized-package-name
          (cond
            (;; if module
            (setq package-name (perlnow-get-package-name-from-module))
            (setq incspot (perlnow-get-incspot package-name file-location))
            (mapconcat 'identity (split-string package-name "::") "-"))
           (;; cpan-style (scripts, etc)
            (setq staging-area (perlnow-find-cpan-style-staging-area))
            (setq hyphenized-package-name (file-name-nondirectory
                                           (perlnow-remove-trailing-slash staging-area))))))
    (cond ((perlnow-script-p)
           (setq test-name-pat "-script\\.t$"))
          (t  ;; better than an explicit (perlnow-module-code-p)
           (setq test-name-pat "\\.t$")))

    (setq testloc-absolute (perlnow-scan-tree-for-t-loc))
    ;; ensure that testloc-absolute exists
    (perlnow-ensure-directory-exists testloc-absolute)

    (setq test-files (directory-files testloc-absolute t test-name-pat nil))
    (cond (test-files
           (let* ((test-files-module
                   (perlnow-grep-list test-files hyphenized-package-name))
                  (test-files-both
                   (perlnow-grep-list test-files-module perlnow-perl-sub-name ))
                  test-files-basename
                  )
             (cond ((and test-files-module (string= perlnow-perl-sub-name "")) 
                    ;; look through "module" list for one with a blank subname.
                    (let* ((blank-subname-any-prefix-pat
                            (concat "-" hyphenized-package-name ".t$"))
                           )
                      (setq test-file
                            (perlnow-latest ;; TODO a silly tie-breaker.  Is biggest better?
                             (perlnow-grep-list test-files-module blank-subname-any-prefix-pat)))
                      )
                    (unless test-file
                      (setq test-file
                            ;; just create a new one (gets sub name from var)
                            (perlnow-new-test-file-name testloc-absolute hyphenized-package-name))
                      ))
                   (test-files-both ;; matches found on module *and* sub: pick latest
                    (setq test-file
                          (perlnow-latest test-files-both))
                    )
                   (t  ;; we're without match on module/sub, so...
                    (cond ((not perlnow-perl-sub-name) ;; no sub name defined
                           (cond (test-files-module   ;; have matches on module: will pick latest
                                  (setq test-file
                                        (perlnow-latest test-files-module))
                                  )
                                 ((perlnow-script-p)
                                  (cond ((setq basename
                                               (file-name-sans-extension
                                                (file-name-nondirectory (buffer-file-name))))

                                         (setq test-files-basename
                                               (perlnow-grep-list test-files basename ))

                                         (setq test-file
                                               (perlnow-latest test-files-basename))
                                         )))
                                 ))
                          (t ;; subname defined
                           (setq test-file
                                 ;; just create a new one (gets sub name from var)
                                 (perlnow-new-test-file-name testloc-absolute hyphenized-package-name))
                           )
                          )
                    ))))
          (t ;; no test files (*and* no subname) pick a new test name
            (setq test-file
                  (perlnow-new-test-file-name testloc-absolute hyphenized-package-name))
            ))
    (setq perlnow-recent-pick test-file)
    (setq perlnow-recent-pick-global test-file)  ;; TODO is this at all useful?
    (if perlnow-trace (perlnow-close-func))
    (if perlnow-debug
        (message "Returning from perlnow-get-test-file-name with test-file: %s" test-file))
    test-file))

(defun perlnow-new-test-file-name (testloc-absolute hyphenized-package-name)
  "Returns a new test file name to be created in TESTLOC-ABSOLUTE.
If there are test files in that location already, will use the maximum prefix
number plus 1, otherwise will use 01.
The name will include the HYPHENIZED-PACKAGE-NAME and, if
available, the `perlnow-perl-sub-name'.
If the current buffer is a script, will use an alternate naming style using
the script basename with the HYPHENIZED-PACKAGE-NAME, if defined."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-new-test-file-name"))
  (if perlnow-debug
      (message "perlnow-new-test-file-name: testloc-absolute: %s  hyphenized-package-name: %s"
               (pp-to-string testloc-absolute) (pp-to-string hyphenized-package-name)))

  (let* ((prefix (perlnow-next-test-prefix testloc-absolute))
         new-name
         )
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
           (let* ((basename (file-name-sans-extension
                             (file-name-nondirectory (buffer-file-name)))) )
             (setq new-name
                   (cond ((and hyphenized-package-name (not (string= hyphenized-package-name "")))
                          ;; 02-Borgia-BananaPeel-deathskate-script.t"
                          (concat testloc-absolute
                                  prefix "-" hyphenized-package-name "-" basename "-script.t"))
                         (t
                          (concat testloc-absolute
                                  prefix "-" basename "-script.t")))
                   )))
          (t  ;; starting context is non-module, non-script
           (setq new-name
                 (concat testloc-absolute prefix "-" hyphenized-package-name ".t") )) )
    (if perlnow-trace (perlnow-close-func))
    new-name))

(defun perlnow-next-test-prefix (testloc-absolute)
  "Look at the test files in TESTLOC-ABSOLUTE and get the next numeric prefix.
If no files are found in TESTLOC-ABSOLUTE, returns 01."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-next-test-prefix"))
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
    (if perlnow-trace (perlnow-close-func))
    next-prefix))

;; The harder-setting *here* is not used for anything.
(defun perlnow-edit-test-file-harder (&optional harder-setting)
  "Open a menu of all likely test files for user to choose from."
  (interactive
   (setq harder-setting (car current-prefix-arg)))
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-edit-test-file-harder"))
  (let ( testloc dotdef namestyle  retval)
    (setq retval
          (perlnow-test-file-menu
           (perlnow-list-perl-tests default-directory)))
    (if perlnow-trace (perlnow-close-func))
    retval))


;;--------
;; test select menu

(defun perlnow-test-file-menu (&optional test-file-list)
  "Show a buffer with a list of test files, allowing the user to
choose one. Defaults to looking up a list of tests using
\\[perlnow-list-perl-tests] run on the `default-directory', but this
may be overridden by passing in a different TEST-FILE-LIST, which should
contain file names with full paths."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-test-file-menu"))
  (let* ((initial-buffer (current-buffer)))
    (save-restriction
      (widen)
      (let* ((md
              (perlnow-connect-t-loc-and-lib)) ;; determines testloc-absolute and incspot by calling metadata
             (original-buffer (current-buffer))
             (original-file   (buffer-file-name))
             (filename (file-name-nondirectory  original-file))
             (menu-buffer-name perlnow-select-test-file-buffer-name) ;; "*select test file*"
             (selection-buffer-label
              (format "Tests from %s. To choose one, cursor to it and hit return."
                      filename)) )
        (unless test-file-list
          (setq test-file-list (perlnow-list-perl-tests default-directory)))
        ;; clear select menu buffer
        (perlnow-show-buffer-other-window menu-buffer-name)
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        ;; insert header line with face color
        (put-text-property
         0 (length selection-buffer-label) 'face 'perlnow-00-face selection-buffer-label)
        (insert selection-buffer-label)
        (insert "\n")
        ;; group sorted list of test names by path in output
        (let* ((sorted-test-file-list (sort test-file-list 'string-collate-lessp))
               (last-path "")
               (first-loop t))
          (dolist (fullfile sorted-test-file-list)
            (let* ((path (file-name-directory    fullfile))
                   ;; (file (file-name-nondirectory fullfile))
                   )
              (cond ((not (equal path last-path))
                     (unless first-loop (insert "\n"))
                     (let ((path-str (concat path ": ")))
                       (put-text-property 0 (length path-str) 'face 'perlnow-01-face path-str)
                       (insert path-str)
                       (insert "\n")
                       (setq first-loop nil)
                       )))
              (let ((str (perlnow-markup-file-with-path fullfile)))
                (insert "   ")
                (put-text-property 0 (length str) 'face 'perlnow-02-face str)
                (insert str)
                (insert "\n") )
              (setq last-path path)
              ))) ;; end dolist fullfile
        (perlnow-select-mode)
        (setq perlnow-associated-code   original-file) ;; connect menu back to generating context
        (setq perlnow-associated-buffer original-buffer) ;; Experimental TODO
        (setq buffer-read-only 't)
        (goto-char (point-min))
        ;; position cursor over the last file that was selected
        (cond ( perlnow-recent-pick-global
                (let* ((lastname (file-name-nondirectory perlnow-recent-pick-global))
                       (lastpath (file-name-directory    perlnow-recent-pick-global)) )
                  (cond ((and
                          (search-forward (concat lastpath ": ") nil t)
                          (search-forward (concat lastname) nil t))))))
              (t
               (next-line 2) ))
        ;; move to beginning of line, then beginning of next file name
        (move-beginning-of-line 1)
        (goto-char (next-single-property-change (point) 'perlnow-file-path))
        ;; restore the other window, but leave the select menu active
        (other-window 1)
        (switch-to-buffer initial-buffer)
        (other-window 1)
        (if perlnow-trace (perlnow-close-func))
        ;; just to return something.
        menu-buffer-name))))

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-markup-file-with-path"))
    (let* (
           (path (file-name-directory    fullfile))
           (name (file-name-nondirectory fullfile))
           (start 0)
           (end (length name))
           )
      (put-text-property start end 'perlnow-file-path path name)
      (if perlnow-trace (perlnow-close-func))
      name))

(defun perlnow-get-path-from-markedup-name (name)
  "Returns the value of the property perlnow-file-path for string NAME.
This only checks the first character in NAME."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-path-from-markedup-name"))
  (let* ((path
          (get-text-property 0 'perlnow-file-path name))
         )
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-forward-hotspot"))
  (let* ((next-loc (next-single-property-change (point) 'perlnow-file-path)))
    (cond (next-loc
           (goto-char next-loc))))
  (unless (get-text-property (point) 'perlnow-file-path)
    (let* ((next-loc (next-single-property-change (point) 'perlnow-file-path)))
      (cond (next-loc
             (goto-char next-loc)))))
  (move-beginning-of-line 1)
  (goto-char (next-single-property-change (point) 'perlnow-file-path))
  (if perlnow-trace (perlnow-close-func)))

(defun perlnow-select-previous-hotspot ()
  "Skip forward to the next file selection \"hotspot\"."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-previous-hotspot"))
  (let* ((prev-loc (previous-single-property-change (point) 'perlnow-file-path)))
    (cond (prev-loc
           (goto-char prev-loc))))
  (unless (get-text-property (1- (point)) 'perlnow-file-path)
    (let* ((prev-loc (previous-single-property-change (point) 'perlnow-file-path)))
      (cond (prev-loc
              (goto-char prev-loc)))))
  (move-beginning-of-line 1)
  (goto-char (next-single-property-change (point) 'perlnow-file-path))
  (if perlnow-trace (perlnow-close-func)))

(defun perlnow-select-file ()
  "Choose the item on the current line."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-file"))
  (let ( initiating-code-file  selected-file  original-context
         newly-selected-buffer  newly-selected-file-name )
    ;; The code buffer the menu was generated from
    (setq initiating-code-file perlnow-associated-code)
    (setq selected-file (perlnow-select-read-full-file-name))
    ;; trace associated pointers back to code being tested
    (if initiating-code-file
        (setq original-context
              (perlnow-follow-associations-to-non-test-code initiating-code-file)))
    ;; open the selection, then set-up pointer back to original code
    (find-file selected-file)
    (setq newly-selected-buffer    (current-buffer))
    (setq newly-selected-file-name (buffer-file-name))
    (if original-context
        (setq perlnow-associated-code  original-context))
    ;; switch to the original, point at the newly opened test file
    (cond (original-context
           (set-buffer (find-buffer-visiting original-context))
           (setq perlnow-associated-code newly-selected-file-name)
           (setq perlnow-recent-pick     newly-selected-file-name)
           (setq perlnow-recent-pick-global  newly-selected-file-name)  ;; TODO experimental
           ))
    ;; make the newly opened buffer active, display original code in parallel
    (switch-to-buffer newly-selected-buffer)
    (if original-context
        (perlnow-show-buffer-other-window (find-buffer-visiting original-context) nil t))
    (if perlnow-trace (perlnow-close-func))
    ;; just to return something, apparently
    selected-file))

;; Used: all over the place
(defun perlnow-current-line-stripped ()
  "Return the current line as a string.
Strips leading and trailing spaces.
;;
Note: from the test menu, returns the filename without
path, leaving the path hidden in a text property.
See: \\[perlnow-select-full-file-from-current-line]."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-current-line-stripped"))
  (save-excursion
    (let (selected-file beg end)
      (setq selected-file (perlnow-current-line))
      ;; trim leading/trailing spaces
      (setq selected-file (perlnow-strip-leading-trailing-spaces selected-file))
      (if perlnow-trace (perlnow-close-func))
      selected-file)))

;; general buffer string utility (did I really have to write one of these? crazy)
(defun perlnow-current-line ()
  "Get the current line from the buffer and return it as a string."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-current-line"))
  (save-excursion
    (save-restriction
      (widen)
      (let (current-line)
        (move-beginning-of-line 1)
        (setq beg (point))
        (move-end-of-line 1)
        (setq end (point))
        (setq current-line (buffer-substring beg end))
        (if perlnow-trace (perlnow-close-func))
        current-line))))

;; general string utility
(defun perlnow-strip-leading-trailing-spaces (str)
  "Remove leading and trailing spaces from the given STR and return it.
Note: literally just affects space characters, not \"whitespace\" in general."
  ;; trim leading/trailing spaces
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-strip-leading-trailing-spaces"))
  (setq str
        (replace-regexp-in-string "^\s+" "" str))
  (setq str
        (replace-regexp-in-string "\s+$" "" str))
  (if perlnow-trace (perlnow-close-func))
  str)

;; Used indirectly by perlnow-select-file
(defun perlnow-select-read-full-file-name ()
  "In select test buffer, tries to read a file name with path from current line.
Returns nil if not inside a *perlnow test select* buffer, or if
no file-name is found on the current line."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-read-full-file-name"))
  (let (selected-file-compact path selected-file)
    (cond ((perlnow-test-select-menu-p)
           (setq selected-file-compact (perlnow-current-line-stripped))
           (cond ((or selected-file-compact (not (string= selected-file-compact "")))
                  (setq path (perlnow-get-path-from-markedup-name selected-file-compact))
                  (setq selected-file (concat path selected-file-compact))))
           ))
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-create-test"))
  (let ( file  path  hyphenized  next-prefix )
    (cond ((not testfile)
           (setq file (perlnow-current-line-stripped))
           (setq path (perlnow-get-path-from-markedup-name file))
           ;; module name from current line, hyphenized
           (setq hyphenized  (perlnow-module-from-t-file file))
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
           )) )
    (setq perlnow-recent-pick        tf-input)
    (setq perlnow-recent-pick-global tf-input)
    (perlnow-open-test-file          tf-input)
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-select-full-file-from-current-line ()
   "Get the testfile name with full path from the \"*select test file*\" buffer."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-select-full-file-from-current-line"))
  (let* ((selected-file-compact (perlnow-current-line-stripped))
         (path (perlnow-get-path-from-markedup-name selected-file-compact))
         (testfile (concat path selected-file-compact)))
    (if perlnow-trace (perlnow-close-func))
    testfile))

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

;; ;; TODO SETUP KEYMAP put keybindings in a setup routine,
;; ;;      and call it from a perlnow-setup (not yet created).

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-display-inc-array"))
  (let* ((inc-path-list (perlnow-all-incspots))
         (display-buffer "*perlnow @INC*")
         (buffer-label
          (format " @INC locations for perl:"))
         (switch-back   nil)
         (original-buffer (current-buffer))
         (original-file   (buffer-file-name))
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
                                    path-str))
                (t
                 ;; no write-access, color gray (34 or 38)
                 (put-text-property 0 (length path-str)
                                    'face 'perlnow-34-face
                                    path-str)))
          ;; hack: makes this a "hotspot"
          (put-text-property 0 (length path)
                             'perlnow-file-path path
                             path-str)
          (setq line (format "   %s\n" path-str))
          (insert line)
          ))
      (deactivate-mark t))  ;; end let buffer-read-only
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
    (if perlnow-trace (perlnow-close-func))
    display-buffer))

;; bind to "return"
(defun perlnow-inc-dired ()
  "Get path off current line, open in dired in other window."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-inc-dired"))
  (let ( initiating-buffer   selected-dir )
    ;; The code buffer the menu was generated from
    (setq initiating-buffer perlnow-associated-code)
    (setq selected-dir (perlnow-current-line-stripped))
    (message "selected-dir: %s" selected-dir)
    (other-window 1) ;; no-op if only one window in frame
    (dired selected-dir)
    (if perlnow-trace (perlnow-close-func))
    ;; just to return something, apparently
    selected-dir))

(defun perlnow-inc-add-inc (incspot)
  "Add a new location to perl's @INC."
  (interactive "GNew location for @INC: ")
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-inc-add-inc"))
  (let* ((selected-dir (perlnow-current-line-stripped))
         (clean-inc  (expand-file-name incspot))
         (clean-spot (substring-no-properties selected-dir))
         )
    (perlnow-add-incspot-to-perl5lib clean-inc clean-spot)
    (perlnow-inc-refresh)
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-inc-remove-inc ()
  "Remove current location from perl's @INC."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-inc-remove-inc"))
  (let ((selected-dir (perlnow-current-line-stripped)))
    (cond (selected-dir
           (perlnow-remove-incspot-from-perl5lib selected-dir)
           (perlnow-inc-refresh))
          (t
           (message "First, position cursor on the entry you want to remove.")
           ))
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-inc-refresh ()
  "Refresh the display of the @INC buffer."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-inc-refresh"))
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
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-latest-test-file"))
  (let ((latest
         (perlnow-latest test-file-list)) )
    (if perlnow-trace
        (message "   Returning from 'latest-test-file'"))
    (if perlnow-trace (perlnow-close-func))
    latest))

;;--------
;; getting the most recently modified file

(defun perlnow-file-mtime (filename)
  "Return the mtime for the given FILENAME.  
If FILENAME is nil, returns 0."
  ;; (if perlnow-trace (perlnow-open-func "Calling " "perlnow-file-mtime"))
  (let ((mtime 0))
    (cond ((perlnow-file-exists-p filename)
           (let* ((attribs    (file-attributes filename) )
                  (mtime-pair (nth 5 attribs) )
                  (mtime-high (nth 0 mtime-pair))
                  (mtime-low  (nth 1 mtime-pair)) )
             (cond ((and (numberp mtime-high) (numberp mtime-low))
                    (setq mtime (+ (* 65536 mtime-high) mtime-low)) )
                   (t
                    (setq mtime 0)) ) )
           ))
    ;; (if perlnow-trace (perlnow-close-func))
    mtime))

(defun perlnow-file-mtime-p (a b)
  "A \"predicate\" to sort files in order of decreasing age."
  ;; (if perlnow-trace (perlnow-open-func "Calling " "perlnow-file-mtime-p"))
  (let ((ret
         (> (perlnow-file-mtime a) (perlnow-file-mtime b)))
        )
    ;; (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-sort-file-list-by-mtime (list)
  "Given the LIST of file names, sorts it by mtime."
  ;; (if perlnow-trace (perlnow-open-func "Calling " "perlnow-sort-file-list-by-mtime"))
  (let* ((sorted-list (sort list 'perlnow-file-mtime-p)) )
    ;; (if perlnow-trace (perlnow-close-func))
    sorted-list))

(defun perlnow-latest (list)
  "Get the most recently modified file, given a LIST of files."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-latest"))
  (let ((most-recent
         (car (perlnow-sort-file-list-by-mtime list))))
    (if perlnow-trace (perlnow-close-func))
    most-recent))

;;--------
;;

;; Used by: perlnow-open-test-file
(defun perlnow-lookup-preferred-perl-mode ()
  "Look-up which perl mode the user prefers.
Examines the alists `interpreter-mode-alist' and
`auto-mode-alist' to see if perl-mode,
cperl-mode \(or perhaps something else entirely?\)
has been chosen as the default to work on perl code."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-lookup-preferred-perl-mode"))
  (let* ( (default "cperl-mode")
          (mode default)
          (interpreter-rule "perl") ;; should match perl or perl5 ;; TODO risk of perlnow-*-mode
          (auto-rule "\[[pP][pP]\]\[[Llm][Llm][Llm]\]")
               ;; regexp to match a regexp containing: [pP][Llm]
          )
    (cond ((setq mode
                 (perlnow-assoc-regexp interpreter-rule interpreter-mode-alist default)))
          ((setq mode
                 (perlnow-assoc-regexp auto-rule auto-mode-alist default)))
          (t
           (setq mode default)))
    (if perlnow-trace (perlnow-close-func))
    mode))

;; Used by: perlnow-lookup-preferred-perl-mode
(defun perlnow-assoc-regexp (pattern alist &optional default)
  "Return first value from ALIST with key that matches PATTERN."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-assoc-regexp"))
  (let ((ret
         (assoc-default pattern alist 'string-match default)))
    (if perlnow-trace (perlnow-close-func))
    ret))

;; Used by: perlnow-test-run-string-harder
(defun perlnow-find-t-directories ()
  "Find 't' directories associated with current file.
Order them in rough order of likely priority. At present, that is
just \"top to bottom\".  Note: this code looks for \"t\" files directly
adjacent to one of the significant levels of the code's path,
it does not, for example, do a full recursive descent from
the project root."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-find-t-directories"))
  (let* ((slash (convert-standard-filename "/"))
         (levels () )
         (t-list () ))
    (cond ((perlnow-module-code-p)
           (let* (
                   (pm-file (buffer-file-name))
                   (pm-location (file-name-directory pm-file))
                   (package-name (perlnow-get-package-name-from-module))
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
              ;;; split up the tail
              (setq levels (split-string tail slash t)) ;; omit-nulls is on
              ;; append levels one at a time to incspot,
              ;; looking for "t" dirs at each level
              (let ((path incspot))
                (dolist (level levels)
                  (setq path (concat path level slash))
                  (setq candidate (perlnow-fixdir (concat path "t")))
                  (if (file-directory-p candidate)
                      (setq t-list (cons candidate t-list)))
                  ))
              ))
          (t ;; assume it's a script then
           (let* ((full-file (buffer-file-name))
                  (location (file-name-directory    full-file))
                  (filename (file-name-nondirectory full-file)))
             (setq candidate (perlnow-fixdir (concat location "../t")))
             (if (file-directory-p candidate)
                 (setq t-list (cons candidate t-list)))
             (setq candidate (perlnow-fixdir (concat location "t")))
             (if (file-directory-p candidate)
                 (setq t-list (cons candidate t-list)))
             )))
    (if perlnow-trace (perlnow-close-func))
    t-list)) ;; end perlnow-find-t-directories


;;=======
;;  perlnow-project-root and the "stepup" functions
;;  as well as the older "scan-tree" functions
;;
(defun perlnow-project-root ( &optional start-point )
  "Find the root of the project tree related to the current buffer.
Begins looking at optional START-POINT or at current buffer's file or location.
Climbs the tree looking for a likely-looking project-root.
Note: the START-POINT may be a file or a directory: where possible it's
better to give it a file."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-project-root"))
  (let ( project-root )
    ;; if an override is defined, just use it and skip everything else.
    (cond (perlnow-project-root-override
           (setq project-root perlnow-project-root-override))
          (t
           (unless start-point (setq start-point (perlnow-current-context)))
           (let ( package-name  incspot  fileloc  loc  t-loc  bin-loc )
             (setq fileloc (file-name-directory start-point))  ;; if start-point has trailing slash, just returns start-point
             (setq project-root
                   (cond
                    ;; this is very reliable, so we check this first
                    ((setq project-root (perlnow-find-cpan-style-staging-area fileloc))
                     )
                    ;; this *might* be less reliable... consider moving it down the stack
                    ((setq project-root (perlnow-find-git-location                fileloc)))
                    ((setq package-name (perlnow-get-package-name-from-module start-point))
                     (setq incspot
                           (perlnow-get-incspot package-name fileloc))
                     (perlnow-expand-path-from-plist perlnow-project-root-from-lib
                                                     (list "$PN_LIB_DIR" incspot)))
                    ((perlnow-script-p start-point)
                     (setq bin-loc
                           (perlnow-stepup-path-to-matching-name start-point
                                                                 perlnow-script-location-names))
                     (perlnow-expand-path-from-plist perlnow-project-root-from-script
                                                     (list "$PN_SCRIPT_DIR" bin-loc)))
                    ((perlnow-test-p start-point)
                     (setq t-loc
                           (perlnow-stepup-path-to-matching-name start-point
                                                                 perlnow-t-location-names))
                     (perlnow-expand-path-from-plist perlnow-project-root-from-t
                                                     (list "$PN_T_DIR" t-loc)))
                    ((perlnow-test-select-menu-p) ;; checks current buffer (expect nil start-point)
                     (let* ((selected-file-compact (perlnow-current-line-stripped))
                            (path (perlnow-get-path-from-markedup-name selected-file-compact))
                            (test-file (concat path selected-file-compact)))
                       (setq t-loc
                             (perlnow-stepup-path-to-matching-name test-file
                                                                   perlnow-t-location-names))
                       (perlnow-expand-path-from-plist perlnow-project-root-from-t
                                                       (list "$PN_T_DIR" t-loc))
                       ))
                    ( ;; working from a directory, noncpan/nongit
                     (setq loc
                           (cond ((file-directory-p start-point)
                                  start-point)
                                 ((perlnow-dired-buffer-dir))  ;; the dired case, when noncpan/nongit
                                 (t
                                  default-directory)))
                     (perlnow-stepup-project-root-via-subdir-names loc))
                    (t
                     (message "WARNING: perlnow-project-root did not get project-root.")
                     nil) ;; TODO use fallback/catchall?
                    ))
             )))
    (if perlnow-debug
        (message "perlnow-project-root returns: %s" project-root))
    (if perlnow-trace (perlnow-close-func))
    project-root))

;; Used by above: perlnow-project-root
(defun perlnow-stepup-project-root-via-subdir-names ( &optional loc )
  "Looks at all levels above the current one trying to find the project-root.
Looks for at least two of the trio of three subdirs.
Returns the lowest parent of the given LOC that meets the criteria.
Note: this is a last ditch criteria, anything else is tried first before
we resort to this."
  (if perlnow-trace
      (perlnow-open-func "Calling " "perlnow-stepup-project-root-via-subdir-names"))
  (unless loc (setq loc default-directory)) ;; a simple CYA maneuver
  (let* ((slash perlnow-slash)
         (home-dir (perlnow-fixdir "$HOME"))
         (home-dir-pat (concat "^" home-dir))
         (script-names  perlnow-script-location-names)
         (t-loc-names   perlnow-t-location-names)
         (lib-names     perlnow-lib-location-names)
         ;; TODO we look any hits from this joint list-- would be better to go for one from each list
         (names (append script-names t-loc-names lib-names))
         (name-payout 1)
         (winning-score 2) )
    (let ( score  level-path  local-dirs  found-path  short-limit )
      ;; a lower limit on allowed length of path string
      (setq short-limit (cond ((string-match home-dir-pat loc)
                               (length home-dir))
                              (t 1)))
      (setq level-path loc)
      (setq found-path
            (catch 'IT
              (while (>= (length level-path) short-limit)
                (setq score 0)
                ;; Examine the level, determine value to add
                ;; loop over the local-dirs, add a point for each match of a likely name
                (setq local-dirs (mapcar 'perlnow-path-tail (perlnow-dirs level-path)))
                (dolist (loco local-dirs)
                  (dolist (nameo names)
                    (cond ((string= loco nameo)
                           (setq score (+ score name-payout))
                           ))
                    )) ;; end dolist nameo, end dolist loco
                (if (>= score winning-score)
                    (throw 'IT level-path))

                (cond ((equal level-path home-dir)
                       (message
                        "WARNING: perlnow-stepup-project-root-via-subdir-names
                         has reached home dir without success: returning nil")
                       (throw 'IT nil)))
                (setq level-path (perlnow-one-up level-path))
                nil)))
      (if perlnow-trace (perlnow-close-func))
      found-path)))

;; This is a simpler alternative to perlnow-scan-tree-for-directory
;; used by above perlnow-project-root
(defun perlnow-stepup-path-to-matching-name (path search-list)
  "Find a name from the SEARCH-LIST in the PATH.
The SEARCH-LIST is a listing of names we're interested in in order of
priority.  The PATH is a full file-system path which is expected to
contain a match at some level for one of the names in SEARCH-LIST.
Returns the path up to and including the level that matched.
If there was no match, returns nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-stepup-path-to-matching-name"))
  (setq path (file-name-directory path))
  (let* ((home-dir (perlnow-fixdir "$HOME"))
         (home-dir-pat (concat "^" home-dir)))
    (let ( home-dir-p  level  trial-path  new-path  short-limit )
      ;; a lower limit on allowed length of path string
      (setq short-limit (cond ((string-match home-dir-pat path)
                               (length home-dir))
                              (t 1)))
      (setq trial-path path)
      (setq new-path
            (catch 'OUT
              (while (>= (length trial-path) short-limit)
                (dolist (name search-list)
                  (setq level (perlnow-path-tail trial-path))
                  (cond ((string= name level)
                         (cond ((equal trial-path home-dir)
                                (message
                                 "WARNING: %s has reached home dir without success: returning nil"
                                 "perlnow-stepup-path-to-matching-name")
                                (setq trial-path nil) ))
                         (throw 'OUT trial-path)
                         ))
                  )
                (setq trial-path (perlnow-one-up trial-path))
                nil)))
      (if perlnow-trace (perlnow-close-func))
      new-path)))

;; (perlnow-stepup-path-to-matching-name "/home/doom/dev/t/newones/blah/" (list "t"))
;;   => "/home/doom/dev/t"
;; (perlnow-stepup-path-to-matching-name "/home/doom/dev/proj_a/script/blah/" (list "bin" "script" "scripts"))
;;  => "/home/doom/dev/proj_a/script"


;; Used by perlnow-project-root
(defun perlnow-expand-path-from-plist (path tags-plist)
  "Substitutes values for place-holders in PATH.
In addition to envars like $HOME, we allow some \"pseudoenvars\"
in the PATH, such as $PN_LIB_DIR and $PN_FILELOC.

This is done by applying a list of key-value pairs supplied in
the form of a plist \(TAGS-PLIST\).

Notes:
 o  the keys should be strings, not symbols
 o  The $ character is treated as an ordinary character,
    not the regexp end-of-line match.
 o  No attempt is made to restrict the keys to allowed values.
 o  Multiple keys may match in sequence with successive
    substitutions performed.

Before returning, \\[perlnow-fixdir] is run on the resulting value.
If a psueudoenvar is used without a value to substitute, warns
and returns nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-expand-path-from-plist"))
  (let* ( keys  val  pat )
    (setq keys (perlnow-plist-keys tags-plist))
    (dolist (key keys)
      (setq val
            (perlnow-remove-trailing-slash
             (lax-plist-get tags-plist key)))
      ;; simple regexp quoting of $ chars in the key
      (setq pat (replace-regexp-in-string "\\$" "\\\\$" key t))
      (setq path (replace-regexp-in-string pat val path t)))
    (setq path (perlnow-fixdir path))
    (if perlnow-trace (perlnow-close-func))
    path))

;; (defvar perlnow-tags-plist ()
;;   "")
;; (setq perlnow-tags-plist
;;   (list "$PN_LIB_DIR"      "/home/doom/tmp/dev/Yow/lib/"
;;         "$PN_T_DIR"        "/home/doom/tmp/dev/Yow/t/"
;;         "$PN_SCRIPT_DIR"   "/home/doom/tmp/dev/Yow/bin/"
;;         "$PN_FILELOC"      "/home/doom/bogosity/yowcakes"
;;         "$PN_PROJECT_ROOT" "/home/doom/tmp/dev/Yow"))

;; (perlnow-expand-path-from-plist "$PN_LIB_DIR/.." perlnow-tags-plist)


;; Simplified version of perlnow-scan-for-pm-loc
(defun perlnow-stepup-for-pm-loc (module-name &optional start-loc)
  "Step upwards looking sideways for a lib directory containing the MODULE-NAME.
Starts looking around START-LOC or if not given, uses \\[perlnow-current-context].
START-LOC may be a file or a directory."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-stepup-for-pm-loc"))
  (unless start-loc (setq start-loc (perlnow-current-context)))
  (let* (
         (start-dir (perlnow-fixdir (file-name-directory start-loc)))
         (slash perlnow-slash  )
         (sep   path-separator )  ;; on unix, ":"
         (home-dir (perlnow-fixdir "$HOME"))
         (home-dir-pat (concat "^" home-dir))
         (lib-names     perlnow-lib-location-names)
         incspot pm-file-relative
         home-dir-p   homeless-path  levels  level  l  i  level-path  local-dirs
         )
    (cond (start-dir
           (setq pm-file-relative ;; e.g.  Mod/Snuff.pm
                 (concat (replace-regexp-in-string "::" slash module-name) ".pm"))
           (if (string-match home-dir-pat start-dir)
               (setq home-dir-p t))
           (setq homeless-path
                 (replace-regexp-in-string home-dir-pat "" start-dir t))
           (setq levels (reverse (split-string homeless-path slash)))
           (setq l (length levels))
           (setq incspot
                 (catch 'CROOK
                   (setq i 0)
                   (while (< i l) ;; for each directory level, bottom to top
                     (setq level (pop levels)) ;; as written, need to remove levels as you go
                     ;; get path to this level
                     (setq level-path
                           (mapconcat 'identity (reverse levels) perlnow-slash))
                     (if home-dir-p
                         (setq level-path (concat home-dir level-path)))
                     ;; loop over the local-dirs, if it looks like a "lib" name, check inside it
                     (setq local-dirs (perlnow-dirs level-path))
                     (dolist (loco (mapcar 'perlnow-path-tail local-dirs))
                       (dolist (libo lib-names)
                         (cond ((string= loco libo) ;; something like "lib"
                                (setq spot (perlnow-fixdir (concat (perlnow-fixdir level-path) libo)))
                                (if (file-exists-p (concat spot pm-file-relative))
                                    (throw 'CROOK spot))))))
                     (setq i (1+ i))
                     ))))
          (t
           (error "start-loc not supplied, and no buffer-file-name")
           ))
    (if perlnow-trace (perlnow-close-func))
    incspot))

;; All current code (as of Sept 2017) instead uses: perlnow-stepup-path-to-matching-name
;; (this was used in perlnow-scan-tree-for-script-loc, perlnow-scan-tree-for-t-loc)
(defun perlnow-scan-tree-for-directory ( start names-list &optional type short-circuit-opt )
  "Scans tree at START for directories with name in TARGET-NAMES.
NAMES-LIST is a list of file names without paths.
Returns a list of all matches (though warns if there's more than one).
Defaults to showing just accessible directories, (with optional
TYPE set to \"d\", you can relax that to all directories).
If SHORT-CIRCUIT-OPT is set, tries to return some obvious candidates,
but makes no attempt to find all."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-scan-tree-for-directory"))
  (if perlnow-debug
      (message "start: %s" (pp-to-string start)))
  (unless type (setq type "a"))
  (let* ( any-names-pat  capture-name-pat  hits   hit-count   pick  )
    ;; generate pattern like: "^(name1|name2|name3)$"
    (setq any-names-pat (mapconcat 'regexp-quote names-list "\\|"))
    (setq capture-name-pat (concat "^\\(" any-names-pat "\\)$"))
    (cond (short-circuit-opt
           (setq hits
                 (perlnow-file-listing start capture-name-pat type))
           ))
    (cond ((not hits)
           (setq hits
                 (perlnow-recursive-file-listing start capture-name-pat type))
           (setq hit-count (length hits))
           (setq hits (reverse hits))  ;; nreverse
           (cond ((> hit-count 1)
                  (message "perlnow-scan-tree-for-directory: returned multiple-hits: ")
                  (let ((i 1))
                    (dolist (h hits)
                      (message "%d: %s" i (pp-to-string h))
                      (setq i (1+ i)))
                    )))
           ))
    (if perlnow-trace (perlnow-close-func))
    hits))

;; Used in key places:
;;   perlnow-test-file-menu
;;   perlnow-guess-run-string
;;   perlnow-edit-test-file-harder

(defun perlnow-list-perl-tests ( &optional file-or-dir )
  "List perl test files found in tree of FILE-OR-DIR.
Uses \\[perlnow-current-context] if FILE-OR-DIR is not specified.
If FILE-OR-DIR is given, but doesn't exist, returns nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-list-perl-tests"))
  (let ( t-loc t-file-pat t-files )
    (unless file-or-dir (setq file-or-dir (perlnow-current-context)))
    (cond ((or (file-directory-p file-or-dir) (file-regular-p file-or-dir))
           (setq t-loc   (perlnow-scan-tree-for-t-loc file-or-dir))
           (setq t-file-pat "\\\.t$")
           (setq t-files
                 (perlnow-recursive-file-listing t-loc t-file-pat "f"))
           (if perlnow-debug
               (message "scan-tree found t-loc: %s" t-loc))
           )
          (t ;; given a loc, but it doesn't exist, ergo, there is nothing there
           (setq t-files nil)
           ))
    (if perlnow-trace (perlnow-close-func))
    t-files))

;; TODO NEXT under development not in use anywhere yet.
;; this is presently a stub that presumes all perl scripts have a *.pl extension. 
(defun perlnow-list-perl-scripts ( &optional file-or-dir )
  "List perl script files found in tree of FILE-OR-DIR.
Uses \\[perlnow-current-context] if FILE-OR-DIR is not specified.
If FILE-OR-DIR is given, but doesn't exist, returns nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-list-perl-scripts"))
  (let ( script-loc script-file-pat script-files )
    (unless file-or-dir (setq file-or-dir (perlnow-current-context)))
    (cond ((or (file-directory-p file-or-dir) (file-regular-p file-or-dir))
           (setq script-loc   (perlnow-scan-tree-for-script-loc file-or-dir))
           (setq script-file-pat "\\\.pl$") ;; TODO NOW LIKE NOW: no file extension case
           (setq script-files
                 (perlnow-recursive-file-listing script-loc script-file-pat "f"))
           (if perlnow-debug
               (message "scan-tree found script-loc: %s" script-loc))
           )
          (t ;; given a loc, but it doesn't exist, ergo, there is nothing there
           (setq script-files nil)
           ))
    (if perlnow-trace (perlnow-close-func))
    script-files))


;;-------
;; The following three functions are still named "scan-tree",
;; though they uses the step-up/step-down algorithm based on the
;; "my 3 sons" assumption now.

;; Used by:
;;   perlnow-list-perl-tests
;;   perlnow-testloc-from-policy
(defun perlnow-scan-tree-for-t-loc ( &optional file-or-dir )
  "Starting from location FILE-OR-DIR, look for the associated test location.
Uses \\[perlnow-current-context] if FILE-OR-DIR is not specified.
If a test location does not exist, this will create one."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-scan-tree-for-t-loc"))
  (unless file-or-dir (setq file-or-dir (perlnow-current-context)))
  ;; scan upwards looking for the project-root,
  ;; then from the project-root, look down for a "t" directory.
  (let ( project-root t-loc )
    (setq project-root (perlnow-project-root file-or-dir))
    (setq t-loc
          (perlnow-find-or-create-project-subdir perlnow-t-location-names
                                                 project-root))
    ;; (if perlnow-debug (message "perlnow-scan-tree-for-t-loc now returning t-loc %s" t-loc))
    (if perlnow-trace (perlnow-close-func))
    t-loc))

;; Used by perlnow-script
;; (once was used by perlnow-list-perl-files-in-project-- now removed)
(defun perlnow-scan-tree-for-script-loc ( &optional loc )
  "Starting from location LOC, look for associated script location
Use the default-directory instead of LOC if it's not specified.
If a script-location does not exists, this will create one."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-scan-tree-for-script-loc"))
  (let ( project-root  script-loc  target-list
                       ;; script-locs
                       )
    (setq project-root (perlnow-project-root loc))
;;          (setq script-locs
;;            (perlnow-scan-tree-for-directory project-root target-list "a" t))
;;          (setq script-loc (pop script-locs))  ;; TODO simplest: choose first in list
    (setq target-list perlnow-script-location-names) ;;  "bin" "script" "scripts"
    (setq script-loc
          (perlnow-find-or-create-project-subdir target-list
                                                 project-root))
    (if perlnow-debug (message "perlnow-scan-tree-for-script-loc now returning script-loc %s" script-loc))
    (if perlnow-trace (perlnow-close-func))
    script-loc))

;; Used by perlnow-module and perlnow-object-module
(defun perlnow-scan-tree-for-lib-loc ( &optional loc )
  "Look for the \"lib\" directory in current tree."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-scan-tree-for-lib-loc"))
  (let (  project-root  lib-loc  package-name  candidate  target-list )
    (cond
     ;; if we're inside a module, just use it's incspot
     ((setq lib-loc (perlnow-incspot-if-module)))
     ;; not in a module, but we can find a project-root
     ((setq project-root (perlnow-project-root loc))
;;       (setq pm-list (perlnow-recursive-file-listing project-root "\\.pm$" "f"))
;;       (setq candidate (car pm-list)) ;; any better way to choose?
;;       (setq package-name
;;             (perlnow-get-package-name-from-module candidate))
;;       (setq lib-loc (perlnow-get-incspot package-name candidate))
      (setq target-list perlnow-lib-location-names) ;;  "lib"
      (setq lib-loc
            (perlnow-find-or-create-project-subdir target-list
                                                   project-root))
      (if perlnow-debug (message "EXP perlnow-scan-tree-for-lib-loc now returning lib-loc %s" lib-loc))
      ))
    (if perlnow-trace (perlnow-close-func))
    lib-loc))

;; Used by above trio:
;; perlnow-scan-tree-for-t-loc, perlnow-scan-tree-for-script-loc, perlnow-scan-tree-for-lib-loc
(defun perlnow-find-or-create-project-subdir ( names-list &optional project-root )
  "Looks for a sub-directory of project-root named in NAMES-LIST.
Looks in PROJECT-ROOT, but if given runs \\[perlnow-project-root].
If sub-directory not found, creates one using first entry in NAMES-LIST.
Returns the full path to the sub-directory."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-find-or-create-project-subdir"))
  (unless project-root
    (setq project-root (perlnow-project-root loc)))
  (let ( loc candidate )
    (setq loc
          (catch 'UP
            (dolist ( name names-list )
              (setq candidate (perlnow-fixdir (concat project-root name)))
              (if perlnow-debug (message "%s trying candidate: %s"
                                         "perlnow-find-or-create-project-subdir" candidate))
              (if (file-directory-p candidate)
                  (throw 'UP candidate))
              )))
    (cond ((not loc)
           (setq loc (perlnow-fixdir (concat project-root (car names-list))))
           (perlnow-ensure-directory-exists loc)
           ))
    (if perlnow-trace (perlnow-close-func))
    loc))

;;========
;;  code-system probes (etc.)

(defun perlnow-how-to-perl ()
  "Define how to run perl for the current buffer.
Gives precedence to the way it's done with the hash-bang line if
that's found at the top of the file \(this preserves whatever
path and options the author of the code intended, e.g. the \"-T\"
flag\).  If that's not found, it uses the contents of the
`perlnow-perl-program' variable, and if that has not been defined
falls back to just \"perl\"."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-how-to-perl"))
  (let* ((perl-from-hashbang (perlnow-hashbang))
         how-to-perl
         )
    (setq how-to-perl
          (cond ((perlnow-perlish-true-p perl-from-hashbang)
                 perl-from-hashbang
                 )
                (perlnow-perl-program
                 perlnow-perl-program)
                (t
                 "perl")))
    (if perlnow-trace (perlnow-close-func))
    how-to-perl))

(defun perlnow-find-git-location ( &optional file-or-dir )
  "Returns the ancestral location where git control begins.
Looks upward from the current file buffer (or the given FILE-OR-DIR)
looking for the location containing a \".git\"."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-find-git-location"))
  (let* ( git-loc ;; the return
         (buffname (buffer-file-name))
         (pnts-file (perlnow-select-read-full-file-name)) ;; nil if not select test buffer
         (input-file
          (or
           file-or-dir buffname pnts-file perlnow-associated-code perlnow-recent-pick-global))
         (target-list (list ".git"))
         (pre-screen-pattern "^[.][^.][^.]+") ;; Just look at dot files
         )
    (setq git-loc
          (cond (input-file
                 (perlnow-find-location-with-target input-file target-list pre-screen-pattern))
                (t nil)))
    (if perlnow-debug
        (message "perlnow-find-cpan-style-git-loc git-loc: %s" git-loc))
    (if perlnow-trace (perlnow-close-func))
    git-loc))

;; Used by : perlnow-guess-run-string, perlnow-project-root, perlnow-get-test-file-name,
;;           perlnow-incspot-from-t,   perlnow-metadata,     perlnow-cpan-style-code-p
(defun perlnow-find-cpan-style-staging-area ( &optional file-or-dir )
  "Determines if the current file buffer is located in an cpan-style tree.
Should return the path to the current cpan-style staging area, or
nil if it's not found.  The staging area is located by searching
upwards from the location of a file to a location with files that
look like a cpan-style project (as currently implemented, it
looks for at least one of \"Makefile.PL\", \"Build.PL\"\ or \"cpanfile\").
This defaults to working on the current buffer's file \(if available\),
but can use the optional FILE-OR-DIR instead (and a directory
should also work as a FILE-OR-DIR). For the special case of a
\"*perlnow select test*\" buffer, it works with a file name extracted
from the buffer."
  ;; Two important cases to cover are:
  ;;   ~/perldev/Horror-Grossout/lib/Horror/Grossout.pm
  ;;   ~/perldev/Horror-Grossout/t/Horror-Grossout.t
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-find-cpan-style-staging-area"))
  (let* ( staging-area ;; the return
         (buffname (buffer-file-name))
         (pnts-file (perlnow-select-read-full-file-name)) ;; nil if not select test buffer
         (input-file
          (or file-or-dir buffname pnts-file perlnow-associated-code perlnow-recent-pick-global))
         (target-list (list "Makefile.PL" "Build.PL" "cpanfile"))
         (prescreen-pat "^[ltMBc]") ;; to constrain listing, but still include:
                                    ;;    lib, t, Makefile.PL, Build.PL, cpanfile
         )
    (setq staging-area
          (cond (input-file
                 (perlnow-find-location-with-target
                  input-file target-list prescreen-pat))
                (t nil)))
    (if perlnow-trace (perlnow-close-func))
    staging-area))

;; Used only by perlnow-find-git-location and perlnow-find-cpan-style-staging-area
(defun perlnow-find-location-with-target ( file-or-dir target-list prescreen-pat )
  "Looks for an ancestor of given FILE-OR-DIR with one of TARGET-FILES.
Uses PRESCREEN-PAT to limit the files that will be checked.
Note: PRESCREEN-PAT should usually match all of TARGET-FILES.
Note: a directory should also work as a FILE-OR-DIR.
If a cpan staging area is found, runs \\[perlnow-cpan-style-build] on it as a
side-effect."
;; TODO if it doesn't match any, should warn, and return nil
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-find-location-with-target"))
  (let* (
         staging-area
         ;; args for directory-files function:
         (dir        "")        ;; candidate directory under examination
         (nopath     nil)       ;; no paths included in file-listing
         (nosort     t  )
         (file-list  () )       ;; file listing of the candidate directory (pre-screened)
         )
    (cond (file-or-dir
           (setq dir (perlnow-fixdir (file-name-directory file-or-dir)))
           ;; Look at dir, and each level above it, stepping up one each time,
           ;; give up when we reach a level that's not "accessible" (just above $HOME?)
           ;; *or* when it's so short it must be root (unix "/").
           (setq staging-area
                 (catch 'UP
                   (while (> (length dir) 1)
                     (setq file-list (directory-files dir nopath prescreen-pat nosort))
                     ;; (if perlnow-debug
                     ;;    (message (format "file-list: %s" (pp-to-string file-list))))
                     (dolist (file file-list)
                       (dolist (target target-list)
                         (if (string= file target)
                             (throw 'UP dir))
                         ))
                     ;; go up a directory level
                     (setq dir (perlnow-fixdir (concat dir "..")))
                     ;; if we can't read files here, give up
                     (if (not (file-accessible-directory-p dir))
                         (throw 'UP nil))
                     ) ;; end while
                   nil)) ;; end setq/catch: ran the gauntlet without success
           )
          (t
           (setq staging-area nil)))
    (if perlnow-trace (perlnow-close-func))
    staging-area))

(defun perlnow-cpan-style-build (staging-area)
  "Does the cpan-style build in the STAGING-AREA (but only if needed).
Specifically, this runs Makefile.PL and/or Build.PL.
Output is appended to the *perlnow-build* window."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-cpan-style-build"))
  ;; Note: relies on naming convention, "perl *.PL" creates target "*".
  (let* (;; (display-buffer-name "*perlnow-build*")
         (builders (list "Makefile.PL" "Build.PL"))
         (return-flag nil))
    (catch 'OUT
      (dolist (builder builders)
        (let* ((build-target      (file-name-sans-extension builder))
               (build-target-full (concat staging-area build-target))
               (builder-full      (concat staging-area builder)) )
          (save-excursion
            (cond ((not (file-newer-than-file-p build-target-full builder-full))
                   (cond ((file-regular-p builder-full)
                           (let ( (default-directory staging-area) )
                             (perlnow-shell-command "perl" builder)
                             (cond ((file-regular-p build-target-full)
                                    (setq return-flag t)
                                    (throw 'OUT return-flag))))))))))))
    (if perlnow-trace (perlnow-close-func))
    return-flag))

(defun perlnow-get-incspot (package-name pm-location)
  "Determine the module root, the place where the package namespace begins.
Given the PACKAGE-NAME \(e.g. \"New::Module\"\),
and the PM-LOCATION \(as an absolute path to the \".pm\" file,
e.g. for \"/home/doom/perldev/Punk/Skunk/New/Module.pm\"\
the PM-LOCATION is \"/home/doom/perldev/Punk/Skunk/New/\"\),
Returns the module root, \(which in this example is:
\"/home/doom/perldev/Punk/Skunk/\"\).
Returns nil if PACKAGE-NAME or PM-LOCATION is nil."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-get-incspot"))
  ;; Example:
  ;;  /home/doom/perldev/Punk/Skunk/New/Module.pm
  ;;  /home/doom/perldev/Punk/Skunk/New/              => number of slashes:  7
  ;;                                New::Module       => double-colon-count: 1
  ;;  /home/doom/perldev/Punk/Skunk/                  The desired incspot
  ;;
  (let (incspot)
    (cond ((or (eq pm-location nil) (eq package-name nil))
           (setq incspot nil))
          (t
           ;; Conditioning pm-location: if there's a trailing .pm, strip the last level
           (if (string-match
                 (concat "^\(.*?" perlnow-slash "\).*?\\.pm$") pm-location)
               (setq pm-location (match-string 1 pm-location)))
           ;; Ensure there's a trailing slash (among other things)
           (setq pm-location (perlnow-fixdir pm-location))

           (let* ((module-terms-list (split-string package-name "::"))
                  (rev-module-terms-list (reverse module-terms-list)) ;; nreverse
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
    (if perlnow-trace (perlnow-close-func))
    incspot))

;; TODO perl versions unfortunately remain not-boring
(defun perlnow-perlversion-old-to-new (given-version)
  "Convert old form of perl version into the new form.
For example, a GIVEN-VERSION might be 5.006 for which the new is 5.6.0
which is more suitable for use as the -b parameter of h2xs.
If given a version that is already in the new style, just
passes it through unchanged."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perlversion-old-to-new"))
  ;; TODO -- the regexps here probably need improvement.
  ;; Get a definitive list of cases of perl versions that it
  ;; should handle, write a unit test, and refactor this
  (let ((old-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)$")
        (new-version-pat "^\\([0-9]\\)\\.\\([0-9][0-9]*\\)\\.\\([0-9][0-9]*\\)")
        major
        mantissa
        minor1
        ret
        )
    (setq ret
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
           ))
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-staging-area (dev-location package-name)
  "Return path to staging area for DEV-LOCATION & PACKAGE-NAME."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-staging-area"))
  (let ((staging-area
         (file-name-as-directory
          (concat
           (perlnow-fixdir dev-location)
           (mapconcat 'identity (split-string package-name "::") "-")))))
    (if perlnow-trace (perlnow-close-func))
    staging-area))

(defun perlnow-full-path-to-cpan-style-module (dev-location package-name)
  "Get the full path to a module created by h2xs.
E.g. if the DEV-LOCATION were \"/usr/local/perldev\" and the PACKAGE-NAME
were \"New::Module\", this should return:
\"/usr/local/perldev/New-Module/lib/New/Module.pm\""
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-full-path-to-cpan-style-module"))
  (let ((pm-file
         (concat
          (file-name-as-directory dev-location)
          (mapconcat 'identity (split-string package-name "::") "-")
          "/lib/"
          (mapconcat 'identity (split-string package-name "::") perlnow-slash)
          ".pm")))
    (if perlnow-trace (perlnow-close-func))
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-full-path-to-dev-test-file"))
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
    (if perlnow-trace (perlnow-close-func))
    test-file))

;; Used by: perlnow-milla, perlnow-module-starter
(defun perlnow-full-path-new-module-starter-test-file (modstar-location package-name)
  "Get the full path to a the new test file to be added to a
structure created by module_starter (using Module::Build).
Follows a very simple fixed policy, given a module named
Modular::Stuff creates a file called 01-Modular-Stuff.t."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-full-path-new-module-starter-test-file"))
  (let* ((hyphenated (mapconcat 'identity (split-string package-name "::") "-"))
         (location (concat (perlnow-fixdir modstar-location) "t" perlnow-slash))
         (filename (format "01-%s.t" hyphenated))
         (fullname (concat location filename)) )
    (if perlnow-trace (perlnow-close-func))
    fullname))

(defun perlnow-export-list-for (package-name &optional incspot-opt)
  "Gets the full export list for the given PACKAGE-NAME.
The package should be located in the given INCSPOT-OPT, though
if the module is installed, that can be omitted."
;; Generates and runs shell commands like:
;;  perl -I'/home/doom/lib' -MCranky::Devil -e 'print join " ", @Cranky::Devil::EXPORT_OK, "\n";' 2>/dev/null
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-export-list-for"))
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
    (if perlnow-trace (perlnow-close-func))
    export-list))

(defun perlnow-inc ()
  "Returns contents of perl's @INC as a list."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-inc"))
  (let* ((perl-inc (shell-command-to-string "perl -e 'foreach (@INC) {print \"$_\t\"}'" ))
         (inc-path-list (split-string perl-inc "\t" t "[ \t\n]"))
         )
    (if perlnow-trace (perlnow-close-func))
    inc-path-list))

(defun perlnow-perl5lib ()
  "Returns contents of the PERL5LIB envar @INC as a list."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perl5lib"))
  (let* ((sep path-separator)  ;; on unix, ":"
         (perl5lib-list (split-string (getenv "PERL5LIB") sep))
         )
    (if perlnow-trace (perlnow-close-func))
    perl5lib-list))

;; TODO could refactor to use perlnow-inc
(defun perlnow-incspot-in-INC-p (&optional incspot)
  "Determine if the INC-SPOT has been included in perl's @INC search path.
If not given a INC-SPOT, it defaults to using the module root of the
current file buffer.  Used by \\[perlnow-do-script-from-module]."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-incspot-in-INC-p"))
  ;; Note: Just checking getenv("PERL5LIB") would be close, but
  ;; using @INC as reported by perl seems more solid, so that's
  ;; what we do here.
  (unless incspot
    (setq incspot
          (perlnow-get-incspot
           (perlnow-get-package-name-from-module)
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
    (if perlnow-trace (perlnow-close-func))
    return))

(defun perlnow-module-found-in-INC (package-name)
  "Given a perl PACKAGE-NAME \(in double-colon separated form\)
return the first module file location found in perl's @INC
array, or nil if it is not found."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-found-in-INC"))
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
    (if perlnow-trace (perlnow-close-func))
    retval))

(defun perlnow-all-incspots ()
  "Return a list of all locations in perl's @INC array."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-all-incspots"))
  (let* ((perl-inc
          (shell-command-to-string
           "perl -e 'print join \"\t\", @INC '" ))
         ;; omit-nulls & trim whitespace
         (inc-path-list (split-string perl-inc "\t" t "[ \t\n]"))
         )
    (if perlnow-trace (perlnow-close-func))
    inc-path-list))


;;;========
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
;;;========

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
  (let* ((restrict-to-word-completion nil)
         (ret
          (perlnow-read-minibuffer-workhorse restrict-to-word-completion)))
    ret))


(defun perlnow-read-minibuffer-complete-word ()
  "Does automatic completion only up to the end of the next \"word\".
As opposed to an entire directory or file name as
\\[perlnow-read-minibuffer-complete\] does.
Used in reading in the name of a perl module name \(which need not
exist already\), where valid name separators are \(\"/\" or \"::\"\)."
  ;; codename: new spacey
  (interactive)
  (let* ((restrict-to-word-completion t)
         (ret
          (perlnow-read-minibuffer-workhorse restrict-to-word-completion)))
    ret))


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
  (let* ((raw-string (buffer-substring-no-properties (point-min) (point-max)))
         (pat ": ")
         (field-start (+ (string-match pat raw-string) (length pat)))
         (string (substring raw-string field-start))
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
  ;; Does the numbering of items in the alist matter one way or another?
  (let ((i (length alist))
         name  new-alist
         )
    (dolist (pair alist)
      (setq name (car pair))
      (setq name (replace-regexp-in-string "\\.pm$" "" name))
      (setq new-alist (cons (cons name i) new-alist))
      (setq i (- i 1))
      )
    (setq new-alist (reverse new-alist)) ;; nreverse
    new-alist))


(defun perlnow-modules-and-dirs-alist (file-system-path pattern)
  "Generate directory listing alist relevant to perl module creation.
Get a directory listing from the given FILE-SYSTEM-PATH, and return
an alist of the file and directory names that match certain criteria:
All the names must match the given PATTERN \(expected
to be of the form \"^leading_fragment\"\).  Further, the filenames
are restricted to being perl module names \(ending in \"*.pm\"\)
which also pass the \\[perlnow-interesting-file-name-p] test
\(though that is probably redundant\).
These are simple file names that do not include the path,
and the values associated with them in the returned alist
are sequential integers."
;;; For extra credit how about stripping the .pm on the file names?
;;; Nope: I can't do that, it messes up "workhorse" as written.
  (let* (;; some directory-files arguments:
         (directory-full-name nil)
         (directory-nosort nil)
         (file-list
          (directory-files
             file-system-path
             directory-full-name
             pattern
             directory-nosort))
         (i 1)  ;; counter to build alist with numeric value
         match-alist
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
    (setq match-alist (reverse match-alist))  ;; nreverse
    match-alist))

(defun perlnow-divide-hybrid-path-and-package-name (string)
  "Divide the hybrid form of a module path into the two components.

    Input:   /home/devster/lib/Project::Vomit
    Output:  /home/devster/lib/  Project::Vomit

Input STRING is expected to be a hybrid file system
path using slashes for the module root name space, and
double colons for the package name space inside of that.
This routine divides it into it's two components, the module root
and module name, which are returned as a two-element list.
This is like \\[perlnow-divide-module-path-dir-and-tail]
except that this treats colons as part package name, not the path."
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
           (setq incspot
                 (file-name-as-directory (match-string 1 string)))
           (setq package-name (match-string 2 string)))
          (t
           (message "Could not separate into module root and name: %s" string)))
    (list incspot package-name)))

(defun perlnow-interesting-file-name-p (string)
  "Is the given file \(or directory name\) be interesting?
Takes a bare filename sans path as the STRING
argument and returns t if it doesn't match the list of
uninteresting filenames patterns, otherwise nil."
;;; TODO
;;; Shouldn't silently use completion-ignored-extensions. Break it out as a defvar
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
        ret )
    (unless (stringp string)
      (error "Expected string in input"))
    (setq ret
          (not (string-match ignore-pat string)))
    ret))

;;-------
;; file and directory manipulation
;; Used by: perlnow-read-minibuffer-workhorse, perlnow-read-minibuffer-completion-help
(defun perlnow-divide-module-path-dir-and-tail (string)
  "Split a file system path into directory and trailing name fragment.
Treats double-colons and slashes as equivalent separators
\(and forbids colons from the name fragment\).
  Given:   \"/home/doom/lib/Taxed::Reb\"
  Return:  \"/home/doom/lib/Taxed::\"    and  \"Reb\"
This is like \\[perlnow-divide-hybrid-path-and-package-name]
except that this treats any colons as part of the path."
  (let* ((slash           perlnow-slash)
         (break-point-pat (concat "[" slash ":][^" slash ":]*$"))  ;; "[/:][^/:]*$"
          )
    (let ( idx   directory   fragment )
      (cond ((setq idx (string-match break-point-pat string))
             (setq directory (substring string 0 (1+ idx)))
             (setq fragment  (substring string (1+ idx) )) )
          (t
           (setq directory nil)
           (setq fragment  string) ))
    (list directory fragment))))


;; An alternate approach:
;;   (setq colonized (replace-regexp-in-string "-" "::" hyphenized))
(defun perlnow-colonize (string)
  "Convert hyphens to double-colon \(\"::\"\).
Returns nil if STRING is nil."
  (cond (string
         (mapconcat 'identity (split-string string "-") "::"))
        (t
         nil)))

(defun perlnow-hyphenize (string)
  "Convert double-colons to hyphens \(\"::\"\).
Returns nil if STRING is nil."
  (cond (string
         (mapconcat 'identity (split-string string "::") "-"))
        (t
         nil)))

(defun perlnow-slashup (string)
  "Convert double-colons to file system separator \(typically, forward slash\).
Returns nil if STRING is nil."
  (let ((slash perlnow-slash))
    (cond (string
         (mapconcat 'identity (split-string string "::") slash))
          (t
           nil))))

(defun perlnow-require-trailing-slash (path)
  "Takes the given string, appends a forward slash to it.
\(Actually, if not in unix-land, appends whatever would make sense to append.\)
This is for those times when \\[perlnow-fixdir] won't quite do,
and you don't feel like re-learning which of the three elisp
commands would do this for you.  Treats a nil as an empty string,
and returns an empty string in either case, because returning the
root directory \"/\" is never going to be what you want.
And unlike perlnow-fixdir, this will not stick you with the
default-directory in those cases."
  (let* ((slash perlnow-slash)
         (trailing-slash-pat (concat slash "$")))
    (cond ((not path)
           (setq path ""))
          ((string= path "")
           (setq path ""))
          ((not (string-match trailing-slash-pat path))
           (setq path (concat path slash))))
    path))

(defun perlnow-remove-trailing-slash (string)
  "Takes the given STRING, removes a trailing slash, if any found.
\(If not in unix-land, goes after the local file-system separator, whatever it is.\)"
  (let* ((slash perlnow-slash)
         (trailing-slash-pat (concat slash "$")))
    (cond ((not string)
           (setq string ""))
          ((string= string "")
           (setq string ""))
          (t
           (setq string
                 (replace-regexp-in-string trailing-slash-pat "" string))
           ))
    string))

;; general utility (not perlnow specific)
(defun perlnow-file-open-p (full-file-name)
  "Given a FULL-FILE-NAME, return t if there's an open buffer for it.
Otherwise, return nil.  The given full file name should be an absolute path."
  (let ((open-p nil))
    (dolist (bfn (mapcar 'buffer-file-name (buffer-list)))
      (if (equal full-file-name bfn)
          (setq open-p t)))
    open-p))

(defun perlnow-buffer-for-file (full-file-name)
  "If FULL-FILE-NAME is already open, return the buffer for it.
Otherwise return nil. The given full file name should be an absolute path.
Note: \\[find-file] also finds already open buffers, but afterwards
you don't know which state it was in."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-buffer-for-file"))
  (let ( buffy )
    (dolist (bf (mapcar 'identity (buffer-list)))
      ;; (message "bf: %s" (pp-to-string bf))
      (if (equal full-file-name (buffer-file-name bf))
          (setq buffy bf)))
    (if perlnow-trace (perlnow-close-func))
    buffy))

;; general utility
(defun perlnow-ensure-directory-exists (dir &optional ask-message)
  "Make sure that given DIR exists, asking if it needs to be created.
Typically DIR should contain the full path to the the directory.
The ASK-MESSAGE default assumes that DIR is a test file location,
but an alternative one can be supplied.
This 'ask' behavior will be suppressed when `perlnow-quiet' is set."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-ensure-directory-exists"))
  (cond ((not (file-directory-p dir))
         (cond (perlnow-quiet
                (perlnow-mkpath dir))
               (t ;; ask before creating
                (if (y-or-n-p
                     (format "Create directory: %s"
                             dir "?"))
                    (perlnow-mkpath dir))
                ))))
  (if perlnow-trace (perlnow-close-func)))

;; general utility
;; unused
(defun perlnow-ensure-file-exists (file template)
  "If the given FILE doesn't exist, creates it using the TEMPLATE."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-ensure-file-exists"))
  (cond ( (not (file-exists-p file))
          (let* ( (location (file-name-directory file))
                  )
            (unless (file-exists-p location)
              (make-directory location t))
            (save-excursion
              (perlnow-create-with-template file template)
              (save-buffer)
              ))))
  (if perlnow-trace (perlnow-close-func))
  )


;;--------
;; file editing (e.g. to add or remove a line from the bashrc include for perl5lib)

(defun perlnow-remove-line-from-file-regexp (file-name regexp)
  "Remove line from file FILE-NAME if it matches REGEXP."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-remove-line-from-file-regexp"))
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
      )
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-add-to-file (line file-name &optional regexp)
  "Add a LINE to FILE-NAME.
If REGEXP is given, will try to add line just before line that
matches given REGEXP.  Defaults to appending at end of file."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-add-to-file"))
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
    (if perlnow-trace (perlnow-close-func))
    ))

;; directory listing
;; Used by perlnow-list-perl-tests (and was used by perlnow-scan-tree-for-directory)
(defun perlnow-recursive-file-listing (start &optional regexp type include-hiddens)
  "Return a list of a tree of files beginning in location START.
The optional REGEXP can be used to filter the returned names, and
the option TYPE can be used to restrict the listing:
   \"f\"  ordinary files
   \"d\"  directories
   \"a\"  accessible directories
Full paths are used in the return, but the REGEXP matches on the
filename without the path, e.g. \"^t$\" could find \"/home/idjit/dev/t\".
If START is nil, returns nil without signalling an error.
Excludes anything named with a leading dot, unless INCLUDE-HIDDENS is t.
Note: this is a wrapper around \\[directory-files-recursively]."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-recursive-file-listing"))
  (unless regexp (setq regexp ""))
  (let ( file-list  skip-pat )
    ;; match paths with .git or RCS in them
    ;; (setq skip-pat "^[.]\\|^RCS$")  ;; TODO break-out as defvar, consider expanding further
    (setq skip-pat "\\(\\(^\\|/\\)[.]\\)\\|\\(\\(^\\|/\\)RCS\\($\\|/\\)\\)")
    (cond ((not start)  ;; told to look nowhere, so we find nothing
           (setq file-list ()))
          (t
           (mapc
            (lambda (item)
              (if (cond
                   ((not type) t) ;; if no type, match all
                   ((string= type "f")
                    (file-regular-p item))
                   ((string= type "d")
                    (file-directory-p item))
                   ((string= type "a")
                    (file-accessible-directory-p item)
                    ))
                  (cond ((and (not include-hiddens)
                              (not (string-match skip-pat item)))
                         (setq file-list (cons item file-list))
                         ))
                ))
            (directory-files-recursively start regexp t))))
    (if perlnow-trace (perlnow-close-func))
    file-list))

;; Used in just perlnow-scan-tree-for-directory (which is currently unused)
(defun perlnow-file-listing (loc &optional regexp type include-hiddens)
  "Return a list of a tree of files located in LOC.
The optional REGEXP can be used to filter the returned names, and
the option TYPE can be used to restrict the listing:
   \"f\"  ordinary files
   \"d\"  directories
   \"a\"  accessible directories
Full paths are used in the return, but the REGEXP matches on the
filename without the path, e.g. \"^t$\" could find \"/home/idjit/dev/t\".
If LOC is nil, returns nil without signalling an error.
Excludes anything named with a leading dot, unless INCLUDE-HIDDENS is t.
Note: this is a wrapper around \\[directory-files]."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-file-listing"))
  (unless regexp (setq regexp ""))
  (let ( file-list  skip-pat )
    ;; match paths with .git or RCS in them
    ;; (setq skip-pat "^[.]\\|^RCS$")  ;; TODO break-out as defvar, consider expanding further
    (setq skip-pat "\\(\\(^\\|/\\)[.]\\)\\|\\(\\(^\\|/\\)RCS\\($\\|/\\)\\)")
    (cond ((not loc)  ;; told to look nowhere, so we find nothing
           (setq file-list ()))
          (t
           (mapc
            (lambda (item)
              (if (cond
                   ((not type) t) ;; if no type, match all
                   ((string= type "f")
                    (file-regular-p item))
                   ((string= type "d")
                    (file-directory-p item))
                   ((string= type "a")
                    (file-accessible-directory-p item)
                    ))
                  (cond ((and (not include-hiddens)
                              (not (string-match skip-pat item)))
                         (setq file-list (cons item file-list))
                         ))
                ))
            (directory-files loc t regexp))))
    (if perlnow-trace (perlnow-close-func))
    file-list))


;;------
;; list manipulation utilities

;; Used by: perlnow-add-incspot-to-perl5lib
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
    (setq lead (reverse new))  ;; nreverse
    (list lead tail)))

;; Used by: perlnow-remove-incspot-from-perl5lib
(defun perlnow-filter-list (string-list filter-regexp)
  "Filter the given list of strings (STRING-LIST) using the FILTER-REGEXP."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-filter-list"))
  (let (new-list)
    (cond (filter-regexp
           (dolist (string string-list)
             (cond ( (not (string-match filter-regexp string))
                     (setq new-list (cons string new-list))
                     )) )
           (setq new-list (reverse new-list)) ) ;; nreverse
          (t
           (setq new-list nil)
           ))
    new-list))

(defun perlnow-grep-list (string-list match-regexp)
  "Search the given list of strings (STRING-LIST) for items matching the MATCH-REGEXP."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-grep-list"))
  (if perlnow-debug
      (message "  perlnow-grep-list: string-list: %s match-regexp: %s"
               (pp-to-string string-list) match-regexp))
  (let (new-list)
    (cond (match-regexp
           (dolist (string string-list)
             (cond ( (string-match match-regexp string)
                     (setq new-list (cons string new-list))
                     )) )
           (setq new-list (reverse new-list)) ) ;; nreverse
          (t
           (setq new-list nil)))
    (if perlnow-trace (perlnow-close-func))
    new-list))

;; TODO replace this with the seq.el seq-uniq function, new with emacs 25.1
(defun perlnow-uniq-list (string-list)
  "Given a list of strings (STRING-LIST) return a list without duplicates.
Order is not preserved."
  (let (last-item new-list)
    (dolist ( item (sort string-list 'string<))
      (unless (equal item last-item)
        (push item new-list))
      (setq last-item item))
    new-list))
;; (perlnow-uniq-list (list "a" "b" "c" "a" "c" "d" ))
;; => ("d" "c" "b" "a")

(defun perlnow-minimum-nonempty-list (list-of-lists)
  "Given a LIST-OF-LISTS returns the shortest list that still contains something.
In the event of a tie in list length, go with the one closest to the beginning of
the LIST-OF-LISTS."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-minimum-nonempty-list"))
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
          (dolist (this-list (reverse nonempties))  ;; nreverse
            (setq len (length this-list))
            (cond ((= len min)
                   (throw 'COLD this-list)
                   )))))
  (if perlnow-trace (perlnow-close-func))
  foundling))


;;--------
;; plist utilities

;; TODO temporary code switch Eventually, remove this, and clean-up code
;; (e.g. get rid of perlnow-plist-keys-string-to-symbol)
(defvar perlnow-stash-string-key-flag t
  "Policy setting: do we use plist keys of symbols or strings?")

(defun perlnow-stash-reload ( &optional stash-file plist-symbol )
  "Reloads a plist from a json file.
The PLIST-SYMBOL defaults to the global: `perlnow-incspot-from-t-plist'
STASH-FILE defaults to: ~/.emacs.d/perlnow/incspot_from_t.json"
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-stash-reload"))
  (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
  (unless stash-file    (setq stash-file perlnow-incspot-from-t-stash-file))

  (cond ((file-exists-p stash-file)
         ;; there are multiple ways a json file can map to lisp datastructures...
         (let* ((json-object-type 'plist)  ;; default 'alist
                (json-array-type  'list)   ;; default 'vector
                ;; None of these actually work   TODO REPORT BUG
                ;;   (json-key-type `symbol)
                ;;   (json-key-type `string)
                ;;   (json-key-type `keyword)
                (input-data (json-read-file perlnow-incspot-from-t-stash-file))
                )
           (cond (perlnow-stash-string-key-flag
                  (set plist-symbol input-data))
                 (t
                  (set plist-symbol (perlnow-plist-keys-string-to-symbol input-data))
                  ;; (setq input-data (eval plist-symbol))
                  ))
           (if perlnow-trace (perlnow-close-func))
           input-data))))

;; Note: this is hack to deal with the fact that I can't convince json-read-file
;; to restore my key-symbol/value-strings structure, it insists on keys as strings
(defun perlnow-plist-keys-string-to-symbol (plist)
  "Go through a plist with key strings and converting them to symbols."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-plist-keys-string-to-symbol"))
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
    (if perlnow-trace (perlnow-close-func))
    new-data))

(defun perlnow-write-plist-file (&optional stash-file plist-symbol)
  "Writes the data from plist to a json file.
The PLIST-SYMBOL defaults to the global: `perlnow-incspot-from-t-plist'
JSON-FILE defaults to: ~/.emacs.d/perlnow/incspot_from_t.json"
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-write-plist-file"))
  (save-excursion
    (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
    (unless stash-file   (setq stash-file perlnow-incspot-from-t-stash-file))
    (let* (
;;            (data
;;             (json-encode (eval plist-symbol)))
;;             ;; better, maybe: json-encode-plist?
           data-string
           data-plist
           )
      (setq data-plist (eval plist-symbol))
      ;; TODO uniquify the data-plist
      (setq data-string (json-encode data-plist)) ;; TODO maybe: json-encode-plist?

      (find-file stash-file)
      (widen)
      (delete-region (point-min) (point-max))
;;      (insert data)
      (insert data-string)
      (insert "\nBLLOORGGG\n")
      (save-buffer)
      ;; TODO close buffer, or just bury it?
      (if perlnow-trace (perlnow-close-func))
      )))

(defun perlnow-stash-put ( keystr value &optional plist-symbol stash-file)
  "Put pair of KEYSTR and VALUE in the plist indicated by optional PLIST-SYMBOL.
The PLIST-SYMBOL defaults to the global `perlnow-incspot-from-t-plist'.
   Example:
     (perlnow-stash-put \"one\" \"alpha\" 'my-special-plist)
If KEYSTR is nil, does nothing and returns nil. If VALUE is nil,
silently converts it to an empty string."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-stash-put"))
  (let ((ret
         (cond (keystr
                (unless value (setq value ""))
                (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
                (unless stash-file   (setq stash-file perlnow-incspot-from-t-stash-file))
                (unless (stringp keystr)
                  (message "perlnow-stash-put: KEYSTR should be a string: %s" (pp-to-string keystr)))
                (cond (perlnow-stash-string-key-flag
                       (set plist-symbol
                            (plist-put (symbol-value plist-symbol) keystr value))
                       )
                      (t ;; use symbols as keys
                       (set plist-symbol
                            (plist-put (symbol-value plist-symbol) (intern keystr) value))
                       ))
                (if perlnow-debug
                    (message
                     (format "PU perlnow-incspot-from-t-plist: %s" (pp-to-string perlnow-incspot-from-t-plist)) t))
                (if perlnow-debug
                    (message
                             (format "perlnow-stash-put: json: %s"
                                     (pp-to-string (json-encode (eval plist-symbol))))
                             t))
                (perlnow-write-plist-file stash-file plist-symbol))
               (t
                nil))))
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-stash-lookup ( keystr &optional plist-symbol stash-file)
  "Look-up string KEYSTR in the plist indicated by optional PLIST-SYMBOL.
The PLIST-SYMBOL defaults to the global `perlnow-incspot-from-t-plist'.
  Example:
  \(setq value
    \(perlnow-stash-lookup \"one\" 'my-special-plist\)\)
If KEYSTR is nil, returns nil.
"
;; TODO document stash-file stuff, along with reload

;; Thought I might do this:
;;   First tries a symbol form of the key, and if that fails tries the raw string.
;; But looking up the symbol form throws a listp error.  Would need to trap this?

  ;; And that, by the way, is a nasty hack to cover for json-read-file never doing
  ;; what I want, irrespective of the json-key-type setting
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-stash-lookup"))
  (let ((ret
         (cond (keystr
                (unless plist-symbol (setq plist-symbol 'perlnow-incspot-from-t-plist))
                (unless stash-file   (setq stash-file   perlnow-incspot-from-t-stash-file))
                (perlnow-stash-reload stash-file plist-symbol)

                (let ((value
                        (or
                         ;; (lax-plist-get (symbol-value plist-symbol) (intern keystr)) ;; listp error (why?)
                         (lax-plist-get (symbol-value plist-symbol) keystr)
                         )
                        ))
                  value))
               (t
                nil))))
    (if perlnow-trace (perlnow-close-func))
    ret))

;; This is like perl's "keys", and it's hard to believe I needed to write this.
(defun perlnow-plist-keys ( plist )
  "Return all keys of the given plist as a list of strings."
;; Step through a list and skipping the even values
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-plist-keys"))
  (let ( (flip t)
         accumulator
         ret )
    (dolist (item plist)
        (cond (flip
                (let (key)
                  (cond ((symbolp item)
                         (setq key (symbol-name item)) ;; symbol-to-string
                         )
                        (t
                         (setq key item)))
                  (push key accumulator))
                (setq flip nil))
              (t ;; not flip
               (setq flip t))
              ))
    (setq ret
          (reverse accumulator))  ;; nreverse
    (if perlnow-trace (perlnow-close-func))
    ret))


;;;========
;; making elisp more like perl

(defun perlnow-perlish-true-p (arg)
  "Return t if perl would call ARG true.
Checks for non-nil and non-empty string and non-zero."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perlish-true-p"))
  (let ((ret
         (cond (arg ;; arg is non-nil
                (cond ((stringp arg)
                       (not (string= arg "")))
                      ((numberp arg)
                       (not (equal arg 0)))
                      (t ;; some other non-nil type
                       t)))
               (t   ;; arg is nil
                nil))
         ))
    (if perlnow-trace (perlnow-close-func))
    ret))


(defun perlnow-file-exists-p (filename)
  "Return t if file FILENAME exists.
Returns nil if FILENAME is an empty string.
If FILENAME is numeric, runs `number-to-string' first."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-file-exists-p"))
  (let (exists)
    (cond (filename ;; filename is non-nil
           (cond ((and (stringp filename) (not (string= filename "")))
                  (setq exists (file-exists-p filename)) )
                 ((numberp filename)
                  (setq exists (file-exists-p (number-to-string filename))) )
                 ))
          (t   ;; filename is nil
           nil))
    (if perlnow-trace (perlnow-close-func))
    exists))






;;========
;; perltidy

(defun perlnow-run-perltidy ()
  "Formats the entire buffer using perltidy.
Refuses to run if the perltidy command isn't found, or if you
aren't currently in a perl-mode.  Preserves the current location
of point."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-run-perltidy"))
  (widen)
  (let* ( (save-location (point))
          (ret
           (cond ((perlnow-perl-mode-p)
                  (perlnow-run-perltidy-on-region (point-min) (point-max))
                  )
                 (t
                  (message "Won't run perltidy: doesn't look like perl mode.")
                  ))) )
    (goto-char save-location)
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-run-perltidy-on-region (start end)
  "Format the region using perltidy.
Running on the entire buffer is more reliable,
see: \\[perlnow-run-perltidy]."
  (interactive "r")
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-run-perltidy-on-region"))
  (widen)
  (let* ((probe
          (format "perltidy --version"))
         (probe-result (shell-command-to-string probe ))
         (perltidy-found-p (string-match "^This is perltidy" probe-result))
         (command
          (format "perltidy --standard-output --standard-error-output"
                  ))
         ret
         )
    (setq ret
          (cond ( perltidy-found-p
                  (shell-command-on-region start end command nil t "*error*")
                  )
                (t
                 (message "perltidy script not found.")
                 ) ))
    (if perlnow-trace (perlnow-close-func))
    ret))

;;=======
;; Insert boilerplate commands.
;;
;; TODO There might be some reason to use skeleton.el or tempo.el for these.
;;
;; Note: the following presume interspersed pod style.
;;
;; Ideally, there would be some way of customizing these, but then,
;; just writing your own routine is easy enough.

;; TODO needs a tail hook: probably want to save-buffer, then regenerate etags
(defun perlnow-insert-sub (&optional name)
  "Insert the framework for a new sub.
Adapts to context and inserts an OOP framework if this
is an OOP module, otherwise, an ordinary sub."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-sub"))
  (let ((ret
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
                ))))
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-insert-method (name)
  "Insert the framework of a perl method definition"
  (interactive "sMethod name: ")
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-method"))
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
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-insert-basic-sub ( name )
  "Insert the framework of a basic (non-OOP) perl sub definition.
A pod block is created before the sub framework, and the NAME is
added to the module's export list (in the :all tag)."
  (interactive "ssub name: ")
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-basic-sub"))
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
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-add-export (subname)
  "For an Exporter-based module, adds SUBNAME to the \":all\" tag.
This depends on some features in the exporter-based template that ships
with perlnow: the %EXPORT_TAGS feature that defines 'all',
and the qw( ) list it uses."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-add-export"))
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
    (if perlnow-trace (perlnow-close-func))
  )))

(defun perlnow-insert-accessors (field)
  "Insert the basic framework for a perl setter and getter,
Presumes href-based objects.  Uses two variables to define the
naming convention for the accessors: \\[perlnow-getter-prefix],
and \\[perlnow-setter-prefix]."
  (interactive "sObject Attribute name: ")
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-accessors"))
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
  (if perlnow-trace (perlnow-close-func))
  ))

;; The following isn't bad... but why not put this in your template.el
;; for OOP perl?
(defun perlnow-insert-new ()
  "Insert a basic framework for a 'new' method"
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-new"))

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
  (if perlnow-trace (perlnow-close-func))
  ))

;;========
;; template expansion definitions

(defun perlnow-insert-spaces-the-length-of-this-string (string)
  "Insert as many spaces as characters in the given STRING.
Used by the template.el expansion PNFS."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-insert-spaces-the-length-of-this-string"))
  (insert
   (make-string (length
                 (file-name-nondirectory string)
                 ) ?\ ))
  (if perlnow-trace (perlnow-close-func))
  )

;; --------
;; date strings

(defun perlnow-american-date ()
  "Return the date in the common American format: MM/DD/YY.
Much derided though it may be.  Note: no leading zeros on MM or DD."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-american-date"))
  (let* (
         (tl (decode-time (current-time)))
         (day   (nth 3 tl))
         (month (nth 4 tl))
         (year  (nth 5 tl))
         (year-str (format "%d" year))
         (year2 (substring year-str 2))
         (merkin-date (format "%d/%d/%s" month day year2))
         )
    (if perlnow-trace (perlnow-close-func))
    merkin-date))

(defun perlnow-full-date ()
  "Return the date in the fully-spelled out American format.
For example: \"August 8, 2009\" (which I realize is not *just* American)."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-full-date"))
  (let* (
         (month (format-time-string "%B"))
         (day   (format "%d" (nth 3 (decode-time (current-time)))))
         (year (format-time-string "%Y"))
         (fulldate (format "%s %s, %s" month day year))
         )
    (if perlnow-trace (perlnow-close-func))
    fulldate))

;;========
;; perl5lib manipulation

(defun perlnow-add-current-pm-incspot-to-perl5lib ()
  "If the current buffer is a perl module, make sure it's incspot is in @INC.
Intended to be used from a hook such as `after-save-hook'."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-add-current-pm-incspot-to-perl5lib"))
  (let* ((file (buffer-file-name))
         (package-name (perlnow-get-package-name-from-module))
         ret )
    (setq ret
          (cond (package-name
                 (let* ((pm-location (file-name-directory file))
                        (incspot     (perlnow-get-incspot package-name pm-location)) )
                   (perlnow-add-incspot-to-perl5lib incspot)
                   ))))
    (if perlnow-trace (perlnow-close-func))
    ret))

(defun perlnow-add-incspot-to-perl5lib (incspot &optional before-spot)
  "Ensures the given INCSPOT is in @INC by modifying the PERL5LIB envar.
This also appends a line to the `perlnow-bashrc-include-file', which if
sourced in your .bashrc file, will make the change to PERL5LIB permanent.
Normally this just appends, but if BEFORE-SPOT is supplied, it will
try to insert just before BEFORE-SPOT."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-add-incspot-to-perl5lib"))
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
              )))
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-remove-incspot-from-perl5lib (incspot)
  "Given an INCSPOT, removes it from the current PERL5LIB if it's found there.
Manipulates just the PERL5LIB envar, and so can't effect other
locations in @INC.  Any line for the INCSPOT found in
`perlnow-bashrc-include-file' will also be removed."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-remove-incspot-from-perl5lib"))
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
    (if perlnow-trace (perlnow-close-func))
    ))

(defun perlnow-set-perl5lib (perl5lib-list)
  "Changes envar PERL5LIB to match the given list.
Deletes blank entries at beginning and end of PERL5LIB
\(I dunno where these come from, but I want them gone\)."
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-set-perl5lib"))
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
    (if perlnow-trace (perlnow-close-func))
    new-lib-str))



;;;=======
;;; cheat commands ("cheat" == automatically fix things so checks pass)

(defun perlnow-revise-export-list ()
  "Find subs defined in the module that are not yet in the
EXPORT lists, and add them to the qw() list associated with %EXPORT_TAGS."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-revise-export-list"))
  (save-restriction
    (widen)
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
                 ))))
      (if perlnow-trace (perlnow-close-func))
      )))

;;========
;; Older (though not quite deprecated) user level creation commands
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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-using-this-module"))
  (save-restriction
    (widen)
    (require 'template)
    (let* ( (pm-file (buffer-file-name))
            (pm-location  (file-name-directory pm-file))
            (package-name (perlnow-get-package-name-from-module))
            (incspot     (perlnow-get-incspot package-name pm-location))
            )
      (unless package-name
        (error "%s" "This file doesn't look like a perl module (no leading package line)."))
      (perlnow-do-script-from-module script package-name incspot)
      (if perlnow-trace (perlnow-close-func))
      )))

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
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-module-two-questions"))
  (require 'template)
  (setq perlnow-perl-package-name package-name) ; global used to pass value into template
  (let ( (filename (perlnow-full-path-to-module incspot package-name)) )
    (perlnow-create-with-template filename perlnow-perl-module-template)
    (if perlnow-trace (perlnow-close-func))
    ))

;;;========
;; The "simple" functions.  Older code that doesn't use template.el.
(defun perlnow-script-simple ()
  "Quickly jump into development of a new perl script.
This is a simple, though inflexible form of \\[perlnow-script].
One advantage: it does not require the template.el package."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-script-simple"))
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
  (perlnow-perlify-this-buffer-simple)
  (if perlnow-trace (perlnow-close-func))
  )


(defun perlnow-perlify-this-buffer-simple ()
  "Turn the current buffer into perl window \(without template.el\).
This is a simple, but inflexible, command that doesn't
require template.el.
It does three things:
   Adds the hashbang line along with a simple header,
   Makes the file executable,
   Goes into cperl-mode using font-lock-mode."
  (interactive)
  (if perlnow-trace (perlnow-open-func "Calling " "perlnow-perlify-this-buffer-simple"))
  (save-restriction
    (widen)

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
      (message "buffer is now perlified")
      (if perlnow-trace (perlnow-close-func))
      )))

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

;;;------
;;; reporting utilities (sigh)

(defun perlnow-list-to-string (listosity)
  "Take a list of strings, turn it into a multiline block of text."
  (let ((textoidal "")
        (indent (make-string 3 32)))
    (dolist (item listosity)
      (setq textoidal (concat textoidal indent item "\n")))
    textoidal))


;;;--------
;;; report/dump of some key buffer-local vars

(defun perlnow-report-status-vars ()
  "Dump status report of key buffer-local variables."
  (interactive)
  (let* (( initial-buffer (current-buffer))
         ( display-buffer (get-buffer-create perlnow-message-buffer-name))
         ( window-size -13 )   ;; number of lines for display-buffer
         ( mess (perlnow-vars-report-string))
         )
  (set-buffer display-buffer)
  (perlnow-blank-out-display-buffer display-buffer t)
  (insert mess)

  ;; Bring the *perlnow* display window to the fore
  ;;   (bottom window of the frame)
  (perlnow-show-buffer-other-window display-buffer window-size t)
  (switch-to-buffer initial-buffer)
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
            (format "---\n")
            (format "perlnow-dev-location: %s\n"
                    perlnow-dev-location)
            (format "perlnow-dev-location-override: %s\n"
                    perlnow-dev-location-override)
            (format "perlnow-project-root-to-dev-location: %s\n"
                    perlnow-project-root-to-dev-location)

            )))
    mess))


;; this routine is needed for some tests and debug messages.
(defun perlnow-report-metadata ( &optional md )
  "Trial run of \\[perlnow-metadata], now with auto-tron!"
  (interactive)
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
    ))


;; DEBUG
(defun perlnow-report-sub-at-point ()
  ""
  (interactive)
  (message "sub: %s" (perlnow-sub-at-point)))

(defun perlnow-run-find-sub ()
  ""
  (interactive)
  (message "sub: %s" (perlnow-find-sub)))


;;========
;;

(defvar perlnow-documentation-undocumented-features t
  "Undocumented features:

  o  \\[perlnow-insert-sub] inserts a block of pod with an =item tag,
     but that tag can be changed with `perlnow-sub-doc-pod'.

  o  \\[perlnow-insert-sub] always adds the sub name to @EXPORT_OK
     list \(on an Exporter-based module\), *except* when the sub is
     named with a leading underscore.

  o  \\[perlnow-display-inc-array] shows the locations in perl's @INC.
     By default this is bound to \"C-c \ *\".

  o  The perlnow standard structure expects a trio of directories under
     a project root (see `perlnow-documentation-coding-standard'), but
     it is possible some variations of this structure could be handled
     by modifying `perlnow-project-root-from-lib', `perlnow-project-root-from-script',
     and/or `perlnow-project-root-from-script': however, this is all UNTESTED.

     A structure like this might be supported with the following customizations:

                               |--- lib
                    |-- code --|
                    |          |--- bin
     project_root --|
                    |
                    |-- t

      \(setq perlnow-project-root-from-lib        \"$PN_LIB_DIR/../..\"\)
      \(setq perlnow-project-root-from-script  \"$PN_SCRIPT_DIR/../..\"\)


  o  These old variables to control default settings still exist but are not used very much:
     `perlnow-pm-location', `perlnow-script-location', `perlnow-dev-location'.


  o  If something seems very confused with a code buffer, running this command might
     clean things up: `perlnow-reset-buffer-locals'


  o  For an exporter-based module, running `perlnow-revise-export-list' might be
     useful to add all routines to the EXPORT_OK list.




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
