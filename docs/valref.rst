.. _valref:

=======================
 Values and References
=======================

:Author: Dave Abrahams
:Author: Joe Groff
:Date: 2013-03-15

**Abstract:** We propose a system that offers first-class support for
both value- and reference- semantics.  By allowing—but not
requiring—(instance) variables, function parameters, and generic
constraints to be declared as ``val`` or ``ref``, we offer users the
ability to nail down semantics to the desired degree without
compromising ease-of-use.

.. Note::

   We are aware of some issues with naming of these new keywords; to
   avoid chaos we discuss alternative spelling schemes in a section at
   the end of this document.

Introduction
============


General Description
===================

The general rule we propose is that most places where you can write
``var`` in today's swift, and also on function parameters, you can
write ``val`` or ``ref`` to request specific semantics.  Writing
``var`` requests the default for a given type.  Non-``class`` types
(``struct``\ s, ``Int``\ s, ``oneof``\ s) default to ``val``
semantics, while ``class``\ es default to ``ref`` semantics.  Because
the current specification already describes the default behaviors, we
will restrict ourselves to discussing the new combinations, such as
``struct`` and ``class`` variables declared with ``ref`` and ``val``
respectively.

Terminology
===========

When we use the term “copy” for non-``class`` types, we are talking
about what traditionally happens on assignment and pass-by-value.
When applied to ``class`` types, “copy” means to call the ``clone()``
method generated due to the user's explicit declaration of conformance
to the ``Cloneable`` protocol.

When we refer to variables being “declared ``val``” or “declared
``ref``,” we mean to include the case of equivalent declarations using
``var`` that request the default semantics for the type.  

Unless otherwise specified, we discuss implementation details such as
“allocated on the heap” as a way of describing operational semantics,
with the understanding that semantics-preserving optimizations are
always allowed.

When we refer to the “value” of a class, we mean the combination of
values of its ``val`` instance variables and the identities of its
``ref`` instance variables.

Variables
=========

Variables can be explicitly declared ``val`` or ``ref``::

    var x: Int  // x is stored by-value
    val y: Int  // just like "var y: Int"
    ref z: Int  // z is allocated on the heap.

    var q: SomeClass          // q is a reference to SomeClass
    ref r: SomeClass          // just like "var r: SomeClass"
    val s: SomeCloneableClass // s is a unique value of SomeCloneableClass

Assignments and initializations involving at least one ``val`` result
in a copy.  Creating a ``ref`` from a ``val`` copies into heap memory::

    ref z2 = x         // z2 is a copy of x's value on the heap
    y = z              // z2's value is copied into y
    
    ref z2 = z         // z and z2 refer to the same Int value
    ref z3 = z.clone() // z3 refers to a copy of z's value

    val t = r          // Illegal unless SomeClass is Cloneable
    ref u = s          // s's value is copied into u
    val v = s          // s's value is copied into v

Instance Variables
==================

Instance variables can be explicitly declared ``val`` or ``ref``::

  struct Foo {
      var x: Int  // x is stored by-value
      val y: Int  // just like "var y: Int"
      ref z: Int  // allocate z on the heap

      var q: SomeClass          // q is a reference to SomeClass
      ref r: SomeClass          // just like "var r: SomeClass"
      val s: SomeCloneableClass // clone() s when Foo is copied
  }

  class Bar : Cloneable {
      var x: Int  // x is stored by-value
      val y: Int  // just like "var y: Int"
      ref z: Int  // allocate z on the heap

      var q: SomeClass          // q is stored by-reference
      ref r: SomeClass          // just like "var r: SomeClass"
      val s: SomeCloneableClass // clone() s when Bar is clone()d
  }

When a value is copied, all of its instance variables declared ``val``
(implicitly or explicitly) are copied.  Instance variables declared
``ref`` merely have their reference counts incremented (i.e. the
refrence is copied).  Therefore, when the defaults are in play, the
are the existing semantic rules are preserved.

The new rules are as follows:

* A non-``class`` instance variable declared ``ref`` is allocated on
  the heap and can outlive its enclosing ``struct``.

* A ``class`` instance variable declared ``val`` will be copied when
  its enclosing ``struct`` or ``class`` is copied.  We discuss below__
  what to do when the ``class`` is not ``Cloneable``.

Arrays
======

Array elements can be explicitly declared ``val`` or ``ref``::

  var x : Int[42]       // an array of 42 integers
  var y : val Int[42]   // an array of 42 integers
  var y : ref Int[42]   // an array of 42 integers-on-the-heap

Semantics of array elements follow those of instance variables.

``oneof``\ s
============

* Semantics of ``oneof`` elements should follow those of instance
  variables.

__ non-copyable_

Function Parameters
===================

Function parameters can be explicitly declared ``val``, or ``ref``::

  func baz(
      x: Int      // x is passed by-value
    , val y: Int  // just like "y: Int"
    , ref z: Int  // allocate z on the heap

    , q: SomeClass               // passing a reference
    , ref r: SomeClass           // just like "var r: SomeClass"
    , val s: SomeCloneableClass) // Passing a copy of the argument

.. Note:: We suggest allowing explicit ``var`` function parameters for
          uniformity.

Semantics of passing arguments to functions follow those of
assignments and initializations: when a ``val`` is involved, the
argument value is copied.

Interaction with `[byref]`
--------------------------

* Is there anything to say here?

Generics
========

In generics, it is possible

* Do we need to say something special about generic classes?  I think not.

.. _non-copyable:

Non-Copyability
===============

A non-``Cloneable`` ``class`` is not copyable.  That leaves us with
several options:

1. Make it illegal to declare a non-copyable ``val``
2. Make non-copyable ``val``\ s legal, but not copyable, thus
   infecting their enclosing object with non-copyability.
3. Like #2, but also formalize move semantics.  All ``val``\ s,
   including non-copyable ones, would be explicitly movable.  Generic
   ``var`` parameters would probably be treated as movable but
   non-copyable.

We favor taking all three steps, but it's useful to know that there
are valid stopping points along the way.

Objective C Interoperability
============================

*Joe to write this section*

Why This Design Beats Rust/C++/etc.
===================================

* We retain the “easy box.”

* Types meant to be reference types with inheritance aren't Cloneable
  by default

Spelling
========

* ``var`` is like ``val``
  * What about ``let``?
  * Should we drop ``let`` for ivars?

* ``ref`` is like ``[byref]``.  
  * We don't think they can be collapsed
  * Should we spell ``[byref]`` differently?  I think at a high level
    it means something like “rebind the name to a new value.”

* Do we want to consider replacing ``struct`` and/or ``class`` with
  new names such as ``valtype`` and ``reftype``?

* Do we need to name the ``val`` and ``ref`` protocols ``Val`` and ``Ref``?
