Swift classes in detail
=======================

Syntax overview
---------------

::

  class MyClass {
    var MyVar : Int
    func f(x : Int) -> Int {
      return MyVar + x
    }
    static func getAMyclass() -> MyClass {
      return new MyClass
    }
    constructor() {
      MyVar = 10
    }
    destructor {
      // Misc finalization
    }
  }

  extension MyClass {
    func g() -> Int {
      return 4
    }
  }

  func f() {
    var x = new MyClass(10)
  }
  
  class MyDerived : MyClass {
    func h() {}
  }
  

Constructing an instance of a class
-----------------------------------
::

  var x = new MyClass(10)

This syntax basically just calls the constructor found by overload resolution
on the given class.

FIXME: There's been some debate whether we should allow default construction
of classes, or constructing a class with ``MyClass(10)`` on an opt-in basis.
We might want to take another look here later.

Inheritance
-----------

A class inherits from at most one other class.  The base class is named in
the same place protocols are named.  All member functions/properties of the
base class can be accessed through an instance of the derived class 
(ignoring access control and shadowing).

Constructors
------------
::

  constructor() {
    MyVar = 10
  }

This syntax defines a constructor, which is called to build an instance of
the object.  

If no constructors are defined, an implicit constructor is defined which
takes no arguments and implicitly initializes all ivars and the base class.

A constructor in the derived class must delegate to a constructor in the
base class.

The exact syntax for initialization of ivars/delegation to the base class
is still sort of an open question. There are a few possible models here:

1. All ivars are implicitly initialized before the body of the constructor.
   Initializers can be provided on the ivar directly if necessary.  This is the
   closest to the current model.  This was discarded & sucks because it doesn't
   allow initialization based on arguments.
2. Some sort of C++-style list of initializers; this was discarded quickly
   during discussion as being both ugly and not as flexible as we would like.
3. Add some sort of init block to constructors, like::

      constructor (x : Int) {
        init {
          myIvar = x
        }
        print(myIvar)
      }

   Capturing self in the init block would be banned.  If an ivar is accessed
   before initialization, or not initialized in the init block, it would be
   automatically "default"-initialized (as-if with an empty argument list) if
   such construction is possible, or an error otherwise. The ivar rules would
   be enforced using CFG analysis.  Delegation to the base class would be in
   the init block; the syntax here is also an open question.  Proposals
   included ``super(1,2)``, ``constructor(1,2)``, ``This(1,2)``,
   ``MyBaseClass(1,2)``.  In the delegation case, the init block would not
   access ``self`` at all, and would perform a delegation call at the end
   (syntax also undecided).

   This approach sucks simply because it's ugly.  There are a couple of
   alternative ways to express this, including some sort of explicit
   ``constructed`` marker, but I'm not sure we can come up with a variant
   which isn't ugly.

4. Implicitly deduce an init block using the CFG::

      constructor (x : Int) {
        // init implicitly begins here
        myIvar = x
        // init implicitly ends here
        myMethod()
      }

   Essentially the same semantic rules as (2), but less ugly (and possibly
   slightly more flexible in the presence of control flow because there
   could be multiple end-points).  The downside here is that it isn't obvious
   at a glance where exactly the split occurs, which could lead to unexpected
   default construction.

Destructors
-----------

A destructor is defined using just the keyword destructor followed by a
brace-stmt.  Destructors can only be defined in classes, and only in the
class declaration itself.  It's a runtime error if the body resurrects the
object (i.e. if there are live reference to the object after the body of
the destructor runs).  Member ivars are destroyed after the body of the
destructor runs.  FIXME: Where exactly do we run the base class destructor?

Member functions and properties
-------------------------------

Like structs, classes have member functions, properties, and ivars.
Unlike structs, member functions and properties are overridable (and use
dynamic dispatch) by default.  Overriding can be disabled with the "final"
attribute.

In a derived class, if you define a member with the same name as a member
of its base class, the base class member is overridden by default.  If the
type doesn't match (what exactly "match" means TBD), it's an error.  This
implies that it's impossible to hide a base class member without explicit
markings.

The override-by-default model requires two attributes to control when it
isn't doing the right thing: "shadow" and "overload".  "overload" means
that the member of the derived class is an overload of the base class member;
all the members from the base class and the derived class are part of overload
resolution.  "shadow" means that the derived class is intentionally shadowing
the base class name; the name from the base class is never found by name lookup
on the derived class.

Accessing overridden members of the base class
----------------------------------------------

Tentatively, ``super.foo()`` accesses foo from the base class, bypassing
dynamic dispatch.

Extensions
----------

Extensions for structs can only contain methods, properties,
and constructors. They always use static dispatch.

Extensions for classes are more flexible in two respects:

1. They can contains ivars: these are essentially baking in language support
   for a side-table lookup.  They must be either default-initializable or have
   an explicit initializer on the variable definition.  The initializer is run
   lazily.
2. Members of extensions of classes can be overridden (?).  Per our discussion
   in the meeting, I thought this model could work, but in retrospect it might
   be way too confusing; if you have a base class X and a derived class Y,
   overriding an extension of X in an extension of Y leads to strange behavior
   depending on whether the extension of Y is loaded (essentially, the same
   weirdness of ObjC categories and linking).

Name lookup for extensions works much the same way that it does for a derived
class, except that rather than base class vs. derived class, it's names from
current extension vs. names from other sources (or something similar to this).
If there's multiple declarations with the same name, it's an error, and the
user has to resolve it with "shadow", and "overload" (where "shadow" only works
for names from other modules; we'll want some other mechanism for name remapping
for protocol implementations).

Constructors in extensions are required to delegate to another constructor. This
is necessary because of access-control etc.  (FIXME: implicit delegation?)
