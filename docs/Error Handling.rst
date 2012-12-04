.. MemoryAndErrorHandlingModel:

Swift Memory and Error Handling Notes
=====================================

The goal of this writeup is to capture ideas around error handling in APIs and
the tradeoffs involved that influenced this design.

Error Handling In Contemporary Languages
----------------------------------------

C and POSIX proffer a mix of "errno" based APIs and APIs that return an error
code explicitly as a result.  While this approach _works_, it has a number of
problems: 1) it is too easy to accidentally ignore an error, 2) using a thread
local integer variable for error handling is non-extensible, and 3) using the
return value as an error code in C forces the logical result of the function to
be returned by reference through an argument pointer.

In contrast, the "obvious" approach to error handling is exception handling as
known from C++, Java, C#, and many other languages.  Exception handling allows
decoupling the logic that produces an error, the (implicitly generated)
logic that propagates the error, and the logic that ultimately handles the error
code.  The implementation model allows a choice of either "zero cost" exceptions
which have a slow error case, or an implicitly generated propagation which slows
down the normal case a bit to make error propagation faster.  That said, there
are a lot of ways to do exception handling wrong.

Exception Specifications
````````````````````````

Exception specifications are difficult to get right.  Adding an concrete
exception specification (i.e., "I only throw T") to a function is a very strong
guarantee that is often difficult to maintain as APIs evolve.  For this reason,
Java has two sorts of exceptions: normal ones that obey exception specifications
and "runtime" exceptions that are exempt from them.  In practice, this loop-hole
makes exception specifications in Java completely useless for reasoning about
the exception behavior of a program, which is one of the reasons that C#
eliminated them completely.

C++'98 has other unfortunate issues with its exception specifications, including
that all functions are assumed to implicitly throw if they have no exception
spec.  Also, its design for empty exception specification (e.g.
"void foo() throw()") is also problematic, and was improved by adding "noexcept"
in C++'11.  

Objective-C is interesting because its exception specifications (i.e. the
presence of an NSError** argument) is binary: a function can return an
error or not, but it isn't encouraged to document *how* APIs can fail in
detailed ways.

Runtime Failures
````````````````

Both Objective-C and Java recognize a difference between general application
errors and "runtime" errors (such as out-of-bound NSArray accesses, or a null
pointer dereference in Java).  As mentioned above, Java allows runtime errors
to avoid exception specifications, but otherwise treats them the same as other
exceptions.

Objective-C handles runtime errors by throwing an Objective-C exception, which
is a somewhat hard failure because very little Objective-C code is exception
safe.  This leads to memory leaks or have other adverse effects, which is not
regarded as recoverable behavior.

A unfortunate aspect of allowing runtime exceptions to be "caught" is that it
means that removing "guard rails" in the language (e.g. turning off array bounds
checks or null pointer dereference checks) can turn a working application (one
that detects, catches, and handles the error) into a broken application (one
that scribbles on garbage memory).


Other Problems with Exception Handling
``````````````````````````````````````

C++'s exception handling model causes many systems applications (e.g., LLVM,
Webkit, and many others) to disable exception handling with -fno-exception.  One
issue is that C++ exception handling violates its own "pay for what you use"
model of C++ by bloating your code with exception tables and RTTI data,
even if you don't actually throw any exceptions.  The fact that C++ has a poor
model to reason about what calls actually throw also leads to pessimization in
the optimizer as it has to assume the worst case about exception edges, leading
to lower performance (even with "zero cost" exceptions) and code bloat compared
to building with -fno-exceptions.

C++ also requires a very specific design style (emphasizing RAII) to make an
application exception safe because it lacks automatic memory management.

Another common reason that C++ code disables exceptions is that they want a more
"disciplined" or "strict" mode for writing their code.  Many people
(particularly at the lower levels of "systems programming" stack) want to know
about and reason about the error handling and propagation behavior of every
error state that can happen, and do not want the implicit propagation aspect of
exceptions.

Finally, because a lot of disables exceptions, many libraries actively avoid
designing them into their APIs.   The STL in particular has very few APIs that
throw exceptions on error cases, and those APIs have non-throwing counterparts.

Error Handling Goals
--------------------

The design of an error handling system has conflicting goals based on the
audience: some programmers don't want to think about error handling logic at
all - yielding a more "scripting language" sort of experience, while some people
want to control every error case and be forced to think about error handling in
depth - yielding a more "disciplined" experience.  Neither of these is "wrong"
or better than the other, they serve different needs and Swift should support
both use cases.

While level of strictness is negotiable and Swift should support multiple
approaches, the error handling behavior of stable *API* is something that must
be considered as strongly as the arguments and return value of the function.  We
consider it a breaking change (and therefore, unacceptable) for API that was
previously guaranteed to never return an error to start returning error codes.

It's worth noting that (other than its syntax) Objective-C achieves these goals
with NSError.  NSError "results" are explicitly part of the signature of a
method, and one cannot be added or removed without changing the selector (a
breaking change).  Clients who don't care about error handling can (and often
do) completely ignore the NSError result of a method call.
 

Swift Error Handling Model
--------------------------

TODO


strict mode, vs sloppy mode.

API means something is strict.


