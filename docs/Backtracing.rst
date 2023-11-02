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
|                 |         | platform.                                        |
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
| cache           | yes     | Set to ``no`` to disable symbol caching.  This   |
|                 |         | only has effect on platforms that have a symbol  |
|                 |         | cache that can be controlled by the runtime.     |
+-----------------+---------+--------------------------------------------------+
| output-to       | stdout  | Set to ``stderr`` to send the backtrace to the   |
|                 |         | standard error instead of standard output.  This |
|                 |         | may be useful in some CI systems.                |
+-----------------+---------+--------------------------------------------------+
| swift-backtrace |         | If specified, gives the full path to the         |
|                 |         | swift-backtrace binary to use for crashes.       |
|                 |         | Otherwise, Swift will locate the binary relative |
|                 |         | to the runtime library, or using ``SWIFT_ROOT``. |
+-----------------+---------+--------------------------------------------------+

(*) On macOS, this defaults to ``no`` rather than ``yes``.

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

System specifics
----------------

Signal Handling
^^^^^^^^^^^^^^^

On macOS and Linux, program crashes are caught using a signal handler. At time of
writing, this is installed for the following signals:

+--------------+--------------------------+-------------------------------------+
| Signal       | Description              | Comment                             |
+====+=========+==========================+=====================================+
|  3 | SIGQUIT | Quit program             |                                     |
+----+---------+--------------------------+-------------------------------------+
|  4 | SIGILL  | Illegal instruction      |                                     |
+----+---------+--------------------------+-------------------------------------+
|  5 | SIGTRAP | Trace trap               |                                     |
+----+---------+--------------------------+-------------------------------------+
|  6 | SIGABRT | Abort program            |                                     |
+----+---------+--------------------------+-------------------------------------+
|  8 | SIGFPE  | Floating point exception | On Intel, integer divide by zero    |
|    |         |                          | also triggers this.                 |
+----+---------+--------------------------+-------------------------------------+
| 10 | SIGBUS  | Bus error                |                                     |
+----+---------+--------------------------+-------------------------------------+
| 11 | SIGSEGV | Segmentation violation   |                                     |
+----+---------+--------------------------+-------------------------------------+

If crash catching is enabled, the signal handler will be installed for any
process that links the Swift runtime.  If you replace the handlers for any of
these signals, your program will no longer produce backtraces for program
failures that lead to the handler you have replaced.

Additionally, the runtime will configure an alternate signal handling stack, so
that stack overflows can be successfully trapped.

Note that the runtime will not install its signal handlers for a signal if it
finds that there is already a handler for that signal.  Similarly if something
else has already configured an alternate signal stack, it will leave that
stack alone.

macOS
^^^^^

The backtracer is not active by default on macOS.  You can enable it by setting
``SWIFT_BACKTRACE`` to ``enable=yes``, which is sufficient if you build your
programs using Xcode.  If you are using some other build tool to build your
program, you will need to sign the program with the entitlement
``com.apple.security.get-task-allow`` in order for the backtracer to work.  This
is the same entitlement you would need to make various other tools work on your
program, so you may already be doing this.  If not, you will need to make a
property list file containing the entitlements you wish to sign your program
with, e.g. ::

  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
  <key>com.apple.security.get-task-allow</key>
  <true/>
  </dict>
  </plist>

and then to sign your program you should do::

  $ codesign --force --sign - --entitlements entitlements.plist \
      /path/to/your/program

Note that programs with the ``com.apple.security.get-task-allow`` entitlement
will not be accepted for distribution in the App Store, and will be rejected by
notarization.  The entitlement is strictly for debugging purposes only and
software should not be shipped to end users with it enabled.

On macOS, we catch crashes and other events using a signal handler.  Once the
backtracer has finished handling the crash, it will allow the crashing program
to continue and crash normally, which will result in the usual Crash Reporter
log file being generated.

Crash catching *cannot* be enabled for setuid binaries.  This is intentional as
doing so might create a security hole.

Other Darwin (iOS, tvOS)
^^^^^^^^^^^^^^^^^^^^^^^^

Crash catching is not enabled for non-macOS Darwin.  You should continue to look
at the system-provided crash logs.

Linux
^^^^^

Frame Pointers
""""""""""""""

The backtracer currently does a simple frame-pointer based unwind.  As a result,
if you compile your code with ``-fomit-frame-pointer``, which is often the
default for release builds on Intel Linux, you may find that you get incomplete
backtraces.

If you wish to get a more complete backtrace, at a small cost in performance,
you can add the compiler flags ``-Xcc -fno-omit-frame-pointer`` when building
your Swift program.

Static Linking Support
""""""""""""""""""""""

For users who statically link their binaries and do not wish to ship the Swift
runtime library alongside them, there is a statically linked copy of
``swift-backtrace``, named ``swift-backtrace-static`` , in the ``libexec``
directory alongside the normal ``swift-backtrace`` binary.

By default, to locate ``swift-backtrace``, the runtime will attempt to look in
the following locations::

    <swift-root>/libexec/swift/<platform>
    <swift-root>/libexec/swift/<platform>/<arch>
    <swift-root>/libexec/swift
    <swift-root>/libexec/swift/<arch>
    <swift-root>/bin
    <swift-root>/bin/<arch>
    <swift-root>

where ``<swift-root>`` by default is determined from the path to the runtime
library, ``libswiftCore``, ``<platform>`` is the name Swift gives to the platform
(in this case most likely ``linux``) and ``<arch>`` is the name Swift uses for
the CPU architecture (e.g. ``x86_64``, ``arm64`` and so on).

When the runtime is statically linked with _your_ binary, the runtime will
instead determine ``<swift-root>`` in the above patterns relative to *your
binary*.  For example, if your binary is installed in e.g. ``/usr/bin``,
``<swift-root>`` would be ``/usr``.

You will therefore need to install a copy of ``swift-backtrace-static``, renamed
to ``swift-backtrace``, in one of the locations above; the simplest option will
often be to put it in the same directory as your own binary.

You can also explicitly specify the value of ``<swift-root>`` using the
environment variable ``SWIFT_ROOT``, or you can explicitly specify the location
of the backtracer using
``SWIFT_BACKTRACE=swift-backtrace=<path-to-swift-backtrace>``.

If the runtime is unable to locate the backtracer, it will allow your program to
crash as it would have done anyway.
