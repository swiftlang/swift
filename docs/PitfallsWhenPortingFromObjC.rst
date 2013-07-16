.. @raise litre.TestsAreMissing

Common Pitfalls when Porting from ObjC
======================

This document describes a list of issues that you might ran into while porting 
Objective-C Apps to Swift. Most of these are current limitations that will 
be resolved in the future. This document might be useful for people playing 
with Swift now.

Protocols Hack
------------

The protocols and classes are in different namespaces in ObjC, but they are 
in the same namespace in Swift. This is why all ObjC protocols are exported 
into Swift with a suffix ``Proto``.

(<rdar://problem/12951769> Import Objective-C protocols into a submodule)

``@optional`` Protocol Methods
------------

Swift does not have a notion of optional protocol methods, so, currently, 
it will require a class to implement all methods declared in a protocol it 
implements. You might be able to bypass this requirement by specifying that 
your class only implements the protocols with some non-optional methods.

(<rdar://problem/14357101> Need "@optional" protocol methods)

``static inline`` Functions 
------------

``static inline`` functions are not exported by the clang module. The main 
reason is that they will not appear in the library at link time and there is 
currently no way of CodeGen-ing them in Swift.

(<rdar://problem/14357109> Need to be able to import "static inline" functions)

Property or Not? 
------------

While Objective-C allows using method call syntax for property accessors and 
dot syntax for "implicit" Objective-C properties, Swift has a clear separation. 
In Swift, properties should be accessed using the dot syntax and "implicit" 
properties can only be accessed by directly calling the setter and getter 
methods. This creates challenges in porting existing code as you will need 
to disambiguate between the two.

(We're expecting that we can get more adoption of ``@property`` throughout 
the system headers.)

Variadic Methods
------------

Variadic methods and constructors are not yet supported. For example, ``NSLog``
will not work. Use ``println`` instead.

Enums
------------

Support for enums is currently in flax and their representation will change. 
Also there are challenges in providing a great porting experience for 
non-modernized APIs. Unnamed enums are imported as ``UInt``, so they might 
need to be casted to the appropriate type when used 
(ex: ``this.includeImageSwitch.state() == NSInteger(NSOnState))``). Named 
enums are imported as structs, whose value field represents the enum's 
value (ex: ``NSRectEdge(CGRectMaxYEdge.value)``). It's a good idea to 
``:print_decl`` the enumerators in the REPL to see how they are represented. 

(<rdar://problem/12640355> Finish "oneof")

UnsafePointer<Type>
------------

When an ObjC API operates on a pointer to a struct, ``UnsafePointer<Type>`` 
will be used to represent the type. 

(Behaves as expected)

Linking
------------

The clang module importer includes stubs for some methods, so you might 
need to link against more frameworks that your code is actually using 
(ex: QuarzCore).

(autolinking + <rdar://problem/12998194> Module importer generates stubs 
for modules I'm not using)