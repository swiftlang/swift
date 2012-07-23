===================
 Swift Expressions
===================

Tuple Literals
==============




Function Call Syntax
====================

Function calls in swift either involve one of the following forms:

 - a unary operator: !x
 - a binary operator: a + b
 - dot syntax: a.b
 - juxaposition of a tuple literal with a function expression: foo(a, b, c)


API Fragility With Named Arguments
==================================

Because we allow calls with tuple arguments, default arguments, named arguments,
and reordering arguments is allowed.  For example, given::

  func foo(a : int = 4, b : int = 5, c : int = 6) { ... }

The following calls to foo are allowed::

  foo(1, 2, 3)                // calls foo(1, 2, 3)
  foo(1, 2)                   // calls foo(1, 2, 6)
  foo(.a = 1, .b = 2, .c = 3) // calls foo(1, 2, 3)
  foo(.b = 1)                 // calls foo(4, 1, 6)
  foo(.c = 3, .b = 2, .a = 1) // calls foo(1, 2, 3)
  foo(.b = 2, 1, 3)           // calls foo(1, 2, 3)

Though weird in this case, given good naming of arguments, the goal is that this
will make it easier to use functions that take a large number of arguments,
similar to Objective-C methods with named arguments.

One general concern about named arguments: the name of the argument becomes a
critical part of the API for the function, and changing the name of an argument
can break clients (for example, renaming the second argument to "d" would break
the last four examples).  Also, unlike C/ObjC, the naming convention for
arguments also needs to be standardized/discussed as much as the name of APIs
themselves.

After extensive discussion, we decided that this is a feature (at least in the
early stages of swift) and can be enhanced later if it becomes an problem in
practice.

Unlike C, Swift functions default to only being visible within the ownership
domain that they are defined in.  By definition, all of the code in a domain can
be changed and updated as code within the domain changes.  This means that
renaming a parameter can be done for non-API in a straight-forward way,
potentially enhanced by good refactoring support to automate any rewrites across
the codebase.

Once a function gets promoted to API (by adding an attribute to the function),
the person promoting the API needs to think about the arguments, and consider
them as crucial as the name of the function itself.  Much of this discussion
happens already for C during API review, because the argument names for C
functions typically become parts of public header files.

Possible improvements
---------------------

What we have now seems like the simplest minimal set of functionality, and we
can extend this in a number of ways in the future if we choose to:

1. We can introduce a [no_named_arguments] attribute, which prevents callers
   from calling the function with named arguments.  This can be useful on simple
   API that doesn't have a useful argument name (e.g. "sin"), and can also be
   useful when promoting a function to API without wanting to publish names for
   the arguments.

2. When promoting a function to API, we can default to not letting
   outside-of-the-domain callers use the named arguments unless an attribute is
   added to specifically opt into it.  This is the same as #1 but gets the
   "default right".

3. We can choose to allow overloading based on argument names (which implies
   that argument names need to be part of mangling or something, ugh).  This
   allows forwarding functions to be put in place if API want to change their
   argument names for some urgent reason.

