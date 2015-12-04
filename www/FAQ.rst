.. @raise litre.TestsAreMissing
.. _FAQ:

Frequently Asked Questions about Swift
======================================

Obviously many more questions can be added.



Do you plan to rewrite the Swift compiler in Swift?
---------------------------------------------------

Not in the short term.  C++ is a very pragmatic language for implementing
compilers, since it has good performance characteristics and allows higher-level
programming idioms than C.

That said, we do expect Swift to be a better language than C++ in a number of ways,
so why don't we implement the compiler itself in Swift?  There are a couple of
reasons that bootstrapping is not a good idea, at least in the short term:

 * This complicates bringup of the compiler, because you have to move both the
   compiled language and the compiler at the same time as the language evolves.
 * We want the language evolution and direction to be driven by general purpose
   programming challenges, not by the specific needs of compiler hackers.  The
   urge to "scratch our own itch" might be too great.

That said, we are writing the runtime library in Swift itself.  We may also
decide to rewrite the compiler in Swift sometime in the distant future when the
language settles down.  At that point, it may be a good opportunity to revisit
previous (internal to the compiler) design decisions, and we do expect and hope
Swift to be a great language for doing many things, including implementing
compilers.


Won't the design and evolution of Swift be warped by being too C++-centric?
---------------------------------------------------------------------------

This is a common question from Objective-C programmers, primarily those who
really dislike C++.  For many reasons you can have hope that Swift
will end up being a great "successor to Objective-C" instead of a "C++
replacement":

 * The compiler team has expert-level knowledge of Objective-C (the language),
   having implemented the compiler for it from the ground-up.  We probably know
   its dark corners better than anyone.
 * The Swift team has broad experience with a number of other programming
   languages, including C/C++/Objective-C, Python, Haskell, Java, JavaScript,
   C#, ...
 * We know C++ well enough to not want to repeat its mistakes.


It turns out that there are a lot of reasons to dislike C++, and those people
who spend a lot of time writing C++ code are some of the most expert people at
explaining its varied faults.  Don't consider use of C++ to be the same as
"love" for it. :)

How can I contribute to Swift?
------------------------------

Contributions to Swift are welcomed and encouraged! Please see the [Contributing to Swift guide](https://swift.org/contributing/).
To give clarity of what is expected of our members, Swift has adopted the code of conduct defined by the Contributor Covenant. This document is used across many open source communities, and we think it articulates our values well. For more, see the [website](https://swift.org/community/#code-of-conduct).

