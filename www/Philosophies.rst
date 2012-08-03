.. _Philosophies:

Pervasive Philosophies Guiding the Swift Language Design
========================================================

This are just notes, in no particular order.

Don't Repeat Yourself
---------------------

Eliminate semicolons, header files, type annotations, @ signs, make properties
simple, etc.

Expressive APIs
---------------

Beyond repetition, it is important to be able to express intent without getting
bogged down in unnecessary syntax.  This really drives at being able to define
expressive APIs, which can be used cleanly and consistently.  The goal for
Swift is not to be more expressive than C++ or Objective-C, it is to be more
expressive than Ruby, Python, or Javascript.

Closures, multiple return values, generics, features for large-scale design
(stolen from ObjC), etc.

Strong Conventions and Consistency
----------------------------------

We want strong "coding conventions", including naming, indentation, etc like 
Objective-C and Java.  It would be great to have a code reformatter like Go
that everyone uses that has no options :).

Batteries Included
------------------

Standard library should be useful out of the box, with functionality comparable
to popular scripting languages.  This includes a variety of "unix level" 
functionality as well as basic data-types.

Familiarity to Programmers in the C Family of Languages
-------------------------------------------------------

"Semicolon and curly brace" langauges are very popular, including C, 
Objective-C, C++ etc, but also Java, Javascript, Ruby, and many other languages.
For this reason, we want the basic expression and statements in Swift to be
similar to these languages and/or an obvious small leap over them when it makes
sense.

This is why we don't want to use python-style intendation rules, for example.

We do deviate when there is a strong reason to do so.  The C declarator syntax
is a complete disaster (write the prototype for 'signal' with no typedefs), so
we must change it.

Innovation is not a Goal Unto Itself
------------------------------------

Being different from all other langauges is not a goal.  Language design
is not a new field.  It is better to borrow good, proven, ideas from other 
languages than it is to be novel in areas that don't need to change.  This
is pragmatic language implementation, not open-ended language research.


Portability
-----------

Swift code should be portable across architectures (unless "unsafe" 
functionality is used) and it should be possible to port the swift compiler
and runtime to run on multiple OS's.

Safe By Default
---------------

Memory safe, but also correct defaults.  Not expose something as public API by
default.


Performance
-----------

Swift is intended for systems programming, and performance matters at every
level (inefficiencies multiply their factors in large systems).  Swift does
"spend" performance, but only when we get something for it.  This is C++'s
"only pay for what you use" concept.

Safety is something that we're willing to spend performance on.  It implies:

 * array bounds checks
 * no undefined behavior
 * automatic memory management

However, you can disable these and can use completely unsafe facilities if you
want (including malloc and free!).

The second part of performance is being able to have a simple mental model when
you're programming, to understand what the cost is.  It is ok that protocols are
completely dynamically dispatched (and thus "slow"), because you can avoid them
if that's not acceptable for your application.

We implemented the compiler optimizer too, so we have realistic expectations of
what it can do and what it can be reasonably expected to do.  We don't want to
rely on a "sufficiently smart" (that will never realistically happen) compiler
optimizer for basic sanity.


Designed for Support System-Level Programming
---------------------------------------------

Statically compiled, runtime checks for safety can be turned off,
performance, no garbage collector required, and light runtime.


Designed for a Modern Context
-----------------------------

Learn from many examples, assume robust compiler optimization based on LLVM (but
expect realistic things, not magic), assume 64-bit targets will be the main
audience (so int being 64-bits is fine).


