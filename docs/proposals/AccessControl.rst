:orphan:

.. title:: Swift Access Control

The most important feature developers have been asking for is access control. This document describes how access control will behave in Swift 1.0.

The general guiding principle:

	**No entity can be defined in terms of another entity that has less accessibility.**
	
There are three levels of accessibility: "private", "internal", and "public". Private entities can only be accessed from within the source file where they are defined. Internal entities can be accessed anywhere within the module they are defined. Public entities can be accessed from anywhere within the module and from any other context that imports the current module.

The names ``public`` and ``private`` have precedent in many languages; ``internal`` comes from C#. In the future, ``public`` may be used for both API and SPI, at which point we may design additional annotations to distinguish the two.

By default, most entities in a source file have ``internal`` accessibility. This optimizes for the most common case—a single-target application project—while not accidentally revealing entities to clients of a framework module.

.. contents:: :local:

Rules
======

Accessibility of a particular entity is considered relative to the current *accessibility context.* The accessibility context of an entity is the current file (if ``private``), the current module (if ``internal``), or the current program (if ``public``). A reference to an entity may only be written within the entity's accessibility context.

If a particular entity is not accessible, it does not appear in name lookup, unlike in C++. However, accessibility does not restrict access to members via runtime reflection (where applicable), nor (in Swift 1.0) does it necessarily restrict visibility of symbols in a linked binary.


Globals and Members
-------------------

A global function, constant, or variable may have any accessibility less than or equal to the accessibility of its type. That is, a ``private`` constant can have ``public`` type, but not the other way around.

Accessors for variables have the accessibility of their associated variable. The setter may be explicitly annotated with an accessibility less than or equal to the accessibility of the variable; this is written as ``private(set)`` or ``internal(set)`` before the ``var`` introducer.

An initializer, method, subscript, or property may have any accessibility less than or equal to the accessibility of its type (including the implicit 'Self' type), with a few additional rules:

- If the type's accessibility is ``private``, the accessibility of members defaults to ``private``. If the type's accessibility is ``internal`` or ``public``, the accessibility of members defaults to ``internal``.

- If a member is used to satisfy a protocol requirement, it must have at least as much accessibility as the protocol conformance; see :ref:`Protocols` below.

- If an initializer is ``@required`` by a superclass, it must have at least as much accessibility as the subclass type itself.

- Accessors for subscripts follow the same rules as accessors for variables.

- A member may be overridden whenever it is accessible.

The implicit memberwise initializer for a struct has the minimum accessibility of all of the struct's stored properties. The implicit no-argument initializer for structs and classes has the same accessibility as the enclosing type.

In Swift 1.0, enum cases always have the same accessibility as the enclosing enum.

Deinitializers are only invoked by the runtime and always have the same accessibility as the enclosing class.


.. _Protocols:

Protocols
---------

A protocol may have any accessibility less than or equal to the accessibility of the protocols it refines. That is, a ``private`` ExtendedWidget protocol can refine an ``public`` Widget protocol, but not the other way around.

The accessibility of a requirement is the accessibility of the enclosing protocol, rather than ``internal``. In Swift 1.0, requirements may not be given less accessibility than the enclosing protocol.

Swift 1.0 does not support private protocol conformances, so for runtime consistency, the accessibility of the conformance of type T to protocol P is equal to the minimum of T's accessibility and P's accessibility; that is, the conformance is accessible whenever both T and P are accessible. This does not change if the protocol is conformed to in an extension.

All members used to satisfy a conformance must have at least as much accessibility as the conformance. This ensures consistency between views of the type; if any member has *less* accessibility than the conformance, then the member could be accessed anyway through a generic function constrained by the protocol.

A protocol may be used as a type whenever it is accessible. A nominal can conform to a protocol whenever the protocol is accessible.


Structs, Enums, and Classes
---------------------------

A struct, enum, or class may be used as a type whenever it is accessible. A struct, enum, or class may be extended whenever it is accessible.

A class may be subclassed whenever it is accessible. A class may have any accessibility less than or equal to the accessibility of its superclass.

Members in an extension have the same default accessibility as members declared within the extended type. However, an extension may be marked with an explicit accessibility modifier (e.g. ``private extension``), in which case the default accessibility of members within the extension is changed to match.

Extensions with explicit accessibility modifiers may not add new protocol conformances, since Swift 1.0 does not support private protocol conformances (see :ref:`Protocols` above).

A type may conform to a protocol with less accessibility than the type itself.


Types
-----

A nominal type's accessibility is the same as the accessibility of the nominal declaration itself. A generic type's accessibility is the minimum of the accessibility of the base type and the accessibility of all generic argument types.

A tuple type's accessibility is the minimum of the accessibility of its elements. A function type's accessibility is the minimum accessibility of its input and return types.

A typealias may have any accessibility less than or equal to the accessibility of the type it aliases. That is, a ``private`` typealias can refer to an ``public`` type, but not the other way around. This includes associated types used to satisfy protocol conformances.


Runtime Guarantees
==================

Non-``public`` members of a class or extension will not be seen by subclasses or other extensions from outside the module. Therefore, members of a subclass or extension will not conflict with or inadvertently be considered to override non-accessible members of the superclass.

Both ``private`` and ``internal`` increase opportunities for devirtualization, though it is still possible to put a subclass of a ``private`` class within the same file.

Most information about a non-``public`` entity still has to be put into a module file for now, since we don't have resilience implemented. This can be improved later, and is no more revealing than the information currently available in the runtime for pure Objective-C classes.


Interaction with Objective-C
----------------------------

If an entity is exposed to Objective-C, most of the runtime guarantees and optimization opportunities go out the window. We have to use a particular selector for members, everything can be inspected at runtime, and even a private member can be overridden in a dynamic subclass. In this case, access control is only useful for discipline purposes.

Members explicitly marked ``private`` are *not* exposed to Objective-C unless they are also marked ``@objc`` (or ``@IBAction`` or similar), even if declared within a class implicitly or explicitly marked ``@objc``.

Any ``external`` or ``internal`` entities will be exposed in the generated header. ``internal`` entities will be marked with a comment. (Ideally, they will also be protected by a macro. Post-1.0, they may be emitted to a separate header.)


Transparent Functions
=====================

Transparent functions are treated just like regular functions for accessibility purposes. That is, within the body of a transparent function, all ``private`` declarations in the current source file are accessible, all ``internal`` declarations in the current module are accessible, and all ``public`` declarations everywhere are accessible. This means that a transparent function can put references to internal entities into the code generated for another module, so the external symbols for these entities cannot be removed.


Unit Tests
==========

Everything being ``internal`` by default poses a problem for unit tests, which may not be able to access particular entities within an application. A workaround is to mark particular members in the application as ``public``, since in most modern Cocoa and Cocoa Touch apps an application's exported entities are not used for anything anyway.


Non-Goals: "class-only" and "protected"
=======================================

This proposal omits two forms of access control commonly found in other languages, a "class-implementation-only" access (often called "private"), and a "class and any subclasses" access (often called "protected"). We chose not to include these levels of access control because they do not add useful functionality beyond ``private``, ``internal``, and ``public``.

"class-only"
  If "class-only" includes extensions of the class, it is clear that it provides no protection at all, since a class may be extended from any context where it is accessible. So a hypothetical "class-only" must already be limited with regards to extensions. Beyond that, however, a "class-only" limit forces code to be declared within the class that might otherwise naturally be a top-level helper or an extension method on another type.
  
  ``private`` serves the proper use case of limiting access to the implementation details of a class (even from the rest of the module!) while not requiring that all of those implementation details be written lexically inside the class.

"protected"
  "protected" access provides no guarantees of information hiding, since any subclass can now access the implementation details of its superclass---and expose them publicly, if it so chooses. This interacts poorly with our future plans for resilient APIs. Additionally, it increases the complexity of the access control model for both the compiler and for developers, and like "class-only" it is not immediately clear how it interacts with extensions.
  
  Though it is not compiler-enforced, members that might be considered "protected" are effectively publicly accessible, and thus should be marked ``public`` in Swift 1.0. They can still be documented as intended for overriding rather than for subclassing, but the specific details of this are best dealt with on a case-by-case basis.


Potential Future Directions
===========================

- Allowing ``private`` or ``internal`` protocol conformances, which are only accessible at compile-time from a particular accessibility context.

- Limiting particular capabilities, such as marking something ``@final(public)`` to restrict subclassing or overriding outside of the current module.

- Allowing the Swift parts of a mixed-source framework to access private headers.

- Not revealing ``internal`` API at all in the generated header of a Swift framework.

- Levels of ``public``, for example ``public("SPI")``.

- Enum cases less visible than the enum.

- Protocol requirements less visible than the protocol.
