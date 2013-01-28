This document summarizes a few email and meeting discussions we have had. Thanks
goes to Jordan for the intial thorough email summary.

===========================
The Generics Syntax Problem
===========================

The problem, as we have frequently discussed, is various other existing goals
in the swift language make parsing the popular C++ syntax for generics even more
difficult in swift. In particular:

1. Very flexible operator overloading. Unary prefix/postfix '<'/'>' are possible
   to define and whitespace sensitive.
2. Types are first-class values. One can define an operator to make
   ``Int < Float`` mean whatever you want.
3. Optional statement separators make generics potentially parse as multiple
   statement expressions.

=====================================
Summary of Solutions And Alternatives
=====================================

Foo<T>, Foo<K, V>
-----------------

Pros:
- Extremely familiar and valuable syntax to C++, Java, and C# programmers.
- Could be backported to Objective-C if you require using object pointer types
  (e.g. ``NSArray<NSString *>``).

Cons:
- Ambiguous in the case of ``x<y>(z)`` and similar, because ``<`` and ``>`` are
  operator characters. This can result in trivial operator related parse errors
  given our operator grammar (for example: ``f(x< y >(z))`` is three expressions
  being passed to ``f()``.
- Not considered matched characters in Unicode (or in many IDEs, because of
  comparison operators).
- Would probably require banning unary '<' and '>' and then relaxing the white
  space sensitivity of '<' and '>' (to make ``x< y >(z)`` work).
- Poor diagnostics and recovery.


Foo(T), Foo(K,V)
----------------

Pros:
- All generic expressions parse as function calls to be later fixed during
  semantic analysis.

Cons:
- Function declarations grammar would need to change to disambiguate curried
  functions from generic functions. For example, to borrow from our variable
  declaration syntax:
  ::
    var  x : Int { return 42 }
    func y : Int { return 42 }
    func z : (arg : Int) -> Int { return arg }
    func g(T : Equatable) : (a : T, b : T) -> Bool { return a == b }
- Potentially confusing. For example: ``f(Int)()`` -- is ``f`` a generic
  function or a higher-order function?
- Difficult with tuple types. (DaveZ: why?)
- Not great for Objective-C (conflict with category declaration syntax).


Foo[T], Foo[K,V]
----------------

Pros:
- Could be backported to Objective-C; types can never be subscripted there.
- Acceptably ambiguous at parse time. Single argument generics may parse as a
  subscript operation, rather than a generic type. This can be fixed during
  semantic analysis.

Cons:
- Chris strongly wants ``[]`` to always means "array".
- Ambiguous in the case of 'Int[3]' for sized array shorthand. Easy to
  disambiguate unless you want to make arrays of a deduced generic type.
- Confusing for 'f[Int]()' -- Is 'f' a generic function or a dictionary of
  blocks?
- Makes static subscript harder (though that's already out with array shorthand,
  and basically has the same problem).
- The ambiguity has extra difficulty with tuple types.
- Only has precedent in Scala.


Foo{T}, Foo{K, V}
-----------------

Pros:
- Can be made trivially unambiguous at parse time.
- Could be backported to Objective-C, may have some problems for interface
  variables:
  ::
    @interface Foo{T} {
      NSArray{T} *Content;
    }
    @end

Cons:
- Parse time disambiguation requires white space sensitivity similar to our
  existing ``[]`` and ``()`` rules.
- Strong and negative reactions by many to the way this looks.


Foo[[T]], Foo[[K,V]]
--------------------

Pros:
- Could be backported to Objective-C

Cons:
- A bit long to type and to read in monospace.
- Ambiguous with array literals: ``f[[Int]]`` parses as ``f[ [Int] ]``
- Possibly ambiguous with C++11 attributes.


Foo^T, Foo^(K,V), Foo!T, Foo!<K,V>, Foo'T, Foo'[K,V], Foo\T, Foo\(K,V), etc
---------------------------------------------------------------------------

Pros:
- Can be trivially unambiguous.
- Allows for shorter syntax in the one generic argument case.

Cons:
- Not at all familiar, not particularly readable (i.e. "punctuation soup").
- Various characters have various problems:
  - Traditional unary operators -- requires stealing another operator from
    swift's otherwise separate and self-consitent operator rules. Not easy for
    Objective-C because most of these already appear as operators.
  - Backslash -- If we ever decide we want line continuations, we're in trouble.
    Not compatible with Objective-C because of line continuations and Unicode
    escapes.
