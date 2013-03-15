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
``var`` in today's swift, you can write ``val`` or ``ref`` to request
specific semantics.  Writing ``var`` requests the default for a given
type.  Types that are currently classified as “value types” by swift
(``struct``\ s, ``Int``\ s, ``oneof``\ s) default to ``val``
semantics, while ``class``\ es default to ``ref`` semantics.  Because
the current specification already describes the default behaviors, we
will restrict ourselves to discussing the new combinations, such as
``struct`` and ``class`` variables declared with ``ref`` and ``val``
respectively.

Instance Variables
==================

A ``struct`` instance

Function Parameters
===================

Generics
========

* Don't forget to cover Vector<ref X> vs. Vector<val X>

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
  * Should we spell ``[byref]`` differently?

* Do we want to consider replacing ``struct`` and/or ``class`` with
  new names such as ``valtype`` and ``reftype``?
