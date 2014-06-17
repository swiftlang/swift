:orphan:

.. title:: Swift Access Control

The most important feature developers have been asking for is access control. This document describes how access control will behave in Swift 1.0.

The general guiding principle:

	**No entity can be defined in terms of another entity that has less accessibility.**
	
There are three levels of accessibility: "private", "internal", and "exported". Private entities can only be accessed from within the source file where they are defined. Internal entities can be accessed anywhere within the module they are defined. Exported entities can be accessed from anywhere within the module and from any other context that imports the current module.

The name ``private`` has precedent in many languages, while ``internal`` comes from C#. "exported" is traditionally called ``public``, but since we may want to use this for both SPI and API in the future this document avoids connotations that may be associated with ``public`` by using ``exported``. *Note: We can decide on a final name later.*

By default, most entities in a source file have ``internal`` accessibility. This optimizes for the most common case—a single-target application project—while not accidentally revealing entities to clients of the module.

.. contents:: :local:

Rules
======

Accessibility is applied relative to the current decl context. If a particular declaration is not accessible, it does not appear in name lookup, unlike in C++.


Globals and Members
-------------------

A global function, constant, or variable may have any accessibility less than or equal to the accessibility of its type. That is, a ``private`` constant can have ``exported`` type, but not the other way around.

Accessors for variables have the accessibility of their associated variable. The setter may be explicitly annotated with an accessibility less than or equal to the accessibility of the variable; for a stored variable, this is written ``internal set`` (without a body) within braces, in the same way as ``willSet`` and ``didSet`` observers. (Observers do not refer to runtime entities and do not have accessibility.)

An initializer, method, subscript, or property may have any accessibility less than or equal to the accessibility of its type (including the implicit 'Self' type), with a few additional rules:

- If an initializer is ``@required`` by a superclass, it must have at least as much accessibility as the initializer it overrides. *Note: should this be implied or written explicitly?*

- If a member is used to satisfy a protocol requirement, it must have at least as much accessibility as the requirement. Note that this restricts the return type from being covariant; if this is desired, the implementer must use a second method. See :ref:`Protocols` below. *Note: should this be implied or written explicitly?*

- An overridden method, subscript, or property does *not* have to have at least as much accessibility as the member it overrides. If it is more restrictive (say, because private types appear in the return type), the base method can be accessed by upcasting. *Note: should the default change to match the member being overridden?*

- Accessors for subscripts follow the same rules as accessors for variables.

- A member may be overridden whenever it is accessible.

The implicit memberwise initializer for a struct has the minimum accessibility of all of the struct's stored properties. The implicit no-argument initializer for structs and classes has the same accessibility as the enclosing type.

Enum cases may have any accessibility less than or equal to the accessibility of their payload type. In particular, they *may* be less accessible than their full type. If the enum is used from a less privileged context, hidden cases must be handled with a "default" in a switch case.

Deinitializers are only invoked by the runtime and always have the same accessibility as the enclosing class.


.. _Protocols:

Protocols
---------

A protocol may have any accessibility less than or equal to the accessibility of the protocols it refines. That is, a ``private`` ExtendedWidget protocol can refine an ``exported`` Widget protocol, but not the other way around.

A protocol requirement may have any accessibility less than or equal to the accessibility of its type, including the implicit 'Self' type (the protocol). The default accessibility of a requirement is the accessibility of the enclosing protocol, rather than ``internal``.

For runtime consistency, the accessibility of the conformance of type T to protocol P is equal to the minimum of T's accessibility and P's accessibility; that is, the conformance is accessible whenever both T and P are accessible. This does not change if the protocol is conformed to in an extension.

Setters for property and subscript requirements may be explicitly given any accessibility less than or equal to the accessibility of the property or subscript itself. By default, they have the same accessibility.

Because the conformance to a protocol is as accessible as the protocol itself, all members used to satisfy the protocol conformance must be as accessible as the particular requirement. Without this restriction, an implementer could use a method with a private return type to satisfy the protocol conformance, and clients of the class would not have access to it without casting to the protocol type. Unlike casting to a superclass, this is not a free operation, and unlike an overridden method, a method that satisfies a protocol requirement does not have to be annotated in any way. Therefore, it is beneficial to communicate this access explicitly.

A protocol may be used as a type whenever it is accessible. A nominal can conform to a protocol whenever the protocol and all of its strict requirements are accessible.


Structs, Enums, and Classes
---------------------------

A struct, enum, or class may be used as a type whenever it is accessible. A struct, enum, or class may be extended whenever it is accessible.

A class may be subclassed whenever it is accessible. A class may have any accessibility less than or equal to the accessibility of its superclass.

An extension is not an entity unto itself and is not considered to have accessibility. The members of an extension have internal accessibility by default.

A type may conform to a protocol with less accessibility than the type itself.


Types
-----

A nominal type's accessibility is the same as the accessibility of the nominal declaration itself. A generic type's accessibility is the minimum of the accessibility of the base type and the accessibility of all generic parameters.

A tuple type's accessibility is the minimum of the accessibility of its elements. A function type's accessibility is the minimum accessibility of its input and return types.

A typealias may have any accessibility less than or equal to the accessibility of the type it aliases. That is, a ``private`` typealias can refer to an ``exported`` type, but not the other way around.


Operators
---------

An operator may have any accessibility. An operator function may have any accessibility less than or equal to the accessibility of its type and its operator.


Imports
-------

Imports within a file are ``private`` by default; that is, they only apply to the current file. Import declarations can be marked as ``internal`` to make them available to the whole module (like a prefix header), or ``exported`` to re-export another framework's interface.

Scoped imports (``import class Foundation.NSURLComponents``) do not affect accessibility, even though they do affect name lookup::

	import class Foundation.NSURLComponents
	
	let components = NSURLComponents(string: "http://apple.com")
	let URL = components.URL // an NSURL
	println(URL.description) // use the NSURL
	
	let URLAgain: NSURL = URL // error!

Accessibility is calculated based on whether another context is *ever* allowed to access an entity, not whether they can *currently* name the entity. Without this interpretation, every module that exposed an NSURL property would have to make Foundation an ``exported`` import.


Runtime Guarantees
==================

Since an extender or subclasser of a type is not necessarily aware of all members of that type, they may inadvertently define members that would naturally conflict with the existing non-``exported`` members. To avoid this, non-``exported`` members should be given distinct vtable slots from ``exported`` members, so that a new member in a subclass or extension will never be treated as an inadvertent override.

Both ``private`` and ``internal`` increase opportunities for devirtualization, though it is still possible to put a subclass of a ``private`` class within the same file.

Most information about a non-``exported`` entity still has to be put into a module file for now, since we don't have resilience implemented. We can scale this back later, and potentially obfuscate property names, etc. This is no worse than the information currently available in the runtime for pure Objective-C classes.


Interaction with Objective-C
----------------------------

If an entity is exposed to Objective-C, most of the runtime guarantees and optimization opportunities go out the window. We have to use a particular selector for members, everything can be inspected at runtime, and even a private member can be dynamically subclassed. In this case, access control is only useful for discipline purposes.

We could consider *not* treating declarations explicitly marked ``private`` as accessible to Objective-C unless they are also marked ``@objc`` (or ``@IBAction`` or similar).

Any ``external`` or ``internal`` entities will be exposed in the generated header. *In the future, we should guard internal entities with a macro and/or have two headers.*


Transparent Functions
=====================

Transparent functions are treated just like regular functions for accessibility purposes. That is, within the body of a transparent function, all ``private`` declarations in the current source file are accessible, all ``internal`` declarations in the current module are accessible, and all ``exported`` declarations everywhere are accessible. This means that a transparent function can put references to internal entities into the code generated for another module, so the external symbols for these entities cannot be removed.

In a world with resilience, this will get a bit more complicated---though a transparent function is within the "accessibility domain" of the current file, it is outside the "resilience domain" of the entire module.


Unit Tests
==========

Everything being ``internal`` by default poses a problem for unit tests, which may not be able to access particular entities within an application. A workaround is to mark particular members in the application as ``exported``, since in most modern Cocoa and Cocoa Touch apps an application's exported entities are not used for anything anyway.


Future Directions
=================

- Limiting particular capabilities, such as marking something ``@final(exported)`` to restrict subclassing or overriding outside of the current module.

- Allowing the Swift parts of a mixed-source framework to access private headers.

- Not revealing ``internal`` API in the generated header of a Swift framework.

- Levels of ``exported``, for example ``exported("SPI")``.
