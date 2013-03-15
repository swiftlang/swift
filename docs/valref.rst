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

Instance Variables
==================

.. Note:: Don't we want to talk about ``oneof``, arrays and their
          elements, tuples and their elements, etc. all in one breath
          here?  We should consider re-phrasing all statements about
          instance variables.

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

__ non-copyable_

Function Parameters
===================

A function parameter declared ``val`` contains a distinct copy of the
argument's value.  A function parameter declared ``ref`` refers to the
same object as the argument.  

There are two obvious choices for non-``class`` instance variables
passed to a ``ref`` parameter:

1. Copy the value to the heap
2. Make it illegal

I'm not sure which is better.

Generics
========

``val`` and ``ref`` are also protocols known to the compiler.  When a 

* Don't forget to cover Vector<ref X> vs. Vector<val X>

.. _non-copyable:

Non-Copyability
===============

Objective C Interoperability
============================

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
