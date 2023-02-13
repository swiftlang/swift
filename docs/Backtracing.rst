Backtracing support in Swift
============================

When things go wrong, it's always useful to be able to get a backtrace showing
where the problem occurred in your program.

Broadly speaking there are three circumstances where you might want a backtrace,
namely:

  * Program crashes
  * Runtime errors
  * Specific user-defined program events

Historically, Swift has tended to lean on operating system crash catching
support for the first two of these, and hasn't really provided any built-in
support for the latter.  This is fine for Darwin, where the operating system
provides a comprehensive system-wide crash catching facility; it's just about OK
on Windows, which also has system-wide crash logging; but it isn't great
elsewhere, in particular on Linux where a lot of server-side Swift programs
currently rely on a separate package to provide them with some level of
backtrace support when errors happen.

What does Swift now support?
----------------------------

Swift now supports:

  * Automatic crash catching and backtrace generation out of the box.
  * Built-in symbolication.
  * A choice of unwind algorithms, including "fast", DWARF and SEH.
  * Interactive(!) crash/runtime error catching.

Crash catching is enabled by default, and won't interfere with any system-wide
crash reporters you might be using.

How do I configure backtracing?
-------------------------------

There is an environment variable, ``SWIFT_BACKTRACE``, that can be used to
configure Swift's crash catching and backtracing support.  The variable should
contain a ``,``-separated list of ``key=value`` pairs.  Supported keys are as
follows:

+-----------------+---------+--------------------------------------------------+
| Key             | Default | Meaning                                          |
+=================+=========+==================================================+
| enable          | yes*    | Set to ``no`` to disable crash catching, or      |
|                 |         | ``tty`` to enable only if stdin is a terminal.   |
+-----------------+---------+--------------------------------------------------+
| demangle        | yes     | Set to ``no`` to disable demangling.             |
+-----------------+---------+--------------------------------------------------+
| interactive     | tty     | Set to ``no`` to disable interaction, or ``yes`` |
|                 |         | to enable always.                                |
+-----------------+---------+--------------------------------------------------+
| color           | tty     | Set to ``yes`` to enable always, or ``no`` to    |
|                 |         | disable.  Uses ANSI escape sequences.            |
+-----------------+---------+--------------------------------------------------+
| timeout         | 30s     | Time to wait for interaction when a crash        |
|                 |         | occurs.  Setting this to ``none`` or ``0s`` will |
|                 |         | disable interaction.                             |
+-----------------+---------+--------------------------------------------------+
| unwind          | auto    | Specifies which unwind algorithm to use.         |
|                 |         | ``auto`` means to choose appropriately for the   |
|                 |         | platform.  Other options are ``fast``, which     |
|                 |         | does a naïve stack walk; and ``precise``, which  |
|                 |         | uses exception handling data to perform an       |
|                 |         | unwind.                                          |
+-----------------+---------+--------------------------------------------------+
| preset          | auto    | Specifies which set of preset formatting options |
|                 |         | to use.  Options are ``friendly``, ``medium`` or |
|                 |         | ``full``.  ``auto`` means to use ``friendly`` if |
|                 |         | interactive, and ``full`` otherwise.             |
+-----------------+---------+--------------------------------------------------+
| sanitize        | preset  | If ``yes``, we will try to process paths to      |
|                 |         | remove PII.  Exact behaviour is platform         |
|                 |         | dependent.                                       |
+-----------------+---------+--------------------------------------------------+
| threads         | preset  | Options are ``all`` to show backtraces for every |
|                 |         | thread, or ``crashed`` to show only the crashing |
|                 |         | thread.                                          |
+-----------------+---------+--------------------------------------------------+
| registers       | preset  | Options are ``none``, ``all`` or ``crashed``.    |
+-----------------+---------+--------------------------------------------------+
| images          | preset  | Options are ``none``, ``all``, or ``mentioned``, |
|                 |         | which only displays images mentioned in a        |
|                 |         | backtrace.                                       |
+-----------------+---------+--------------------------------------------------+
| limit           | 64      | Limits the length of the captured backtrace. See |
|                 |         | below for a discussion of its behaviour.  Can be |
|                 |         | set to ``none`` to mean no limit.                |
+-----------------+---------+--------------------------------------------------+
| top             | 16      | Specify a minimum number of frames to capture    |
|                 |         | from the top of the stack.  See below for more.  |
+-----------------+---------+--------------------------------------------------+
| swift-backtrace |         | If specified, gives the full path to the         |
|                 |         | swift-backtrace binary to use for crashes.       |
|                 |         | Otherwise, Swift will locate the binary relative |
|                 |         | to the runtime library, or using ``SWIFT_ROOT``. |
+-----------------+---------+--------------------------------------------------+

(*) On macOS, this defaults to ``tty`` rather than ``yes``.

Backtrace limits
----------------

The limit settings are provided both to prevent runaway backtraces and to allow
for a sensible backtrace to be produced even when a function has blown the stack
through excessive recursion.

Typically in the latter case you want to capture some frames at the top of the
stack so that you can see how the recursion was entered, and the frames at the
bottom of the stack where the actual fault occurred.

1. There are ``limit`` or fewer frames.  In this case we will display all
   the frames in the backtrace.  Note that this _includes_ the case where there
   are exactly ``limit`` frames.

2. There are more than ``limit`` frames.

   a. ``top`` is ``0``.  We will display the first ``limit - 1`` frames followed
      by ``...`` to indicate that more frames exist.

   b. ``top`` is less than ``limit - 1``.  We will display ``limit - 1 - top``
      frames from the bottom of the stack, then a ``...``, then ``top`` frames
      from the top of the stack.

   c. ``top`` is greater or equal to ``limit - 1``.  We will display ``...``,
      followed by ``limit - 1`` frames from the top of the stack.

For example, let's say we have a stack containing 10 frames numbered here 1 to
10, with 10 being the innermost frame.  With ``limit`` set to 5, you would see::

  10
  9
  8
  7
  ...

With ``limit`` set to 5 and ``top`` to 2, you would instead see::

  10
  9
  ...
  2
  1

And with ``limit`` set to 5 and ``top`` to 4 or above, you would see::

  ...
  4
  3
  2
  1

What is the swift-backtrace binary?
-----------------------------------

``swift-backtrace`` is a program that gets invoked when your program crashes.
We do this because when a program crashes, it is potentially in an invalid state
and there is very little that is safe for us to do.  By executing an external
helper program, we ensure that we do not interfere with the way the program was
going to crash (so that system-wide crash catchers will still generate the
correct information), and we are also able to use any functionality we need to
generate a decent backtrace, including symbolication (which might in general
require memory allocation, fetching and reading remote files and so on).

You shouldn't try to run ``swift-backtrace`` yourself; it has unusual
requirements, which vary from platform to platform.  Instead, it will be
triggered automatically by the runtime.
