:orphan:

:Author: Joe Groff
:Date: 2014-12-11
:Edition: 1

I've been thinking about a set of features that together should meet
Foundation's needs to build a great successor to KVO for Swift, while also
generally improving our expressiveness for functional programming. I think we
can achieve our goals without introducing any totally new language features; we
only need to extend some of our existing features in natural ways. In
particular, I propose:

- Allowing functions and closures to return ``inout``
- Extending the ``.member`` contextual lookup syntax to work with instance
  properties and methods in function type context, providing functional
  access to properties and self-unbound partial application of methods
- Introducing ``@thin`` function values (function values without captures) and
  ``@static`` function values (function references known at compile-time to
  refer to a specific function, method, or thin literal closure) as building
  blocks for abstractions with memory management and/or ObjC interop
  constraints

I'm presenting these three ideas together because, although they're individually
interesting, I think they're all complementary; it's the composition of these
three ideas together that's really compelling, not only for our immediate
framework needs but for our users in general. (There's also a nonzero chance
I'm crazy and/or misguided here and should change course before getting too
wrapped up in this approach.)

1. Binding and Projection with Inout Returns
============================================

Many applications want to be able to capture full read and write access to a
variable or property. In particular, Foundation needs this functionality for
KVO2, where they want to be able to bind storage to an event source to be
automatically updated. Currently the only way to do this is by manually
capturing closures that get and set the property, e.g.::

  struct Ref<T> {
    var getter: () -> T, setter: T -> ()

    var value: T {
      get { return getter() }
      set { setter(newValue) }
    }
  }

  var x = 0
  let xRef = Ref(getter: { x }, setter: { x = $0 })
  xRef.value = 2
  print(x)

which no one would ever want to write by hand. I propose that we extend the
``inout`` modifier so that it can also be applied to return types, and
likewise, extend the prefix ``&`` operator to be usable in a return or
single-expr closure expression. This isn't as crazy as it may sound at first; a
subscript is essentially a function that takes arguments (its base and index)
and returns inout already. By extending this power to functions, a closure or
standalone function would also be able to produce an lvalue, potentially
capturing local variables in the process::

  var x = 0
  let xRef = { &x }
  xRef() = 2
  print(x)

When combined with our existing ``@autoclosure`` and ``assignment``
operator features, we can write a "bind" operator with implicit capture
semantics::

  infix operator <- { assignment precedence 90 }

  func <- <T> (property: @autoclosure () -> inout T, stream: Stream<T>) {
    // Update the property every time a new value comes from the stream.
    stream.each { property() = $0 }
  }

  var x = 0
  // Increase x by one every second.
  x <- RepeatTimer(1.seconds).accumulate(0) { $0 + 1 }

We can also write projection functions, to apply a common access path to
multiple bases::

  var building: [Int: Apartment] = [
    101: Apartment(resident: Person()),
    503: Apartment(resident: Person())
  ]

  func residentNameByNumber(number: Int) -> inout String {
    return &building[number]!.resident.name
  }

  residentNameByNumber(503) = "Jane Smith"

And mutable operators could be defined, for instance to dereference
pointers::

  // Pascal-flavor
  postfix operator ^ {}
  // C-flavor
  prefix operator * {}

  postfix func ^ <T> (x: UnsafeMutablePointer<T>) -> inout T {
    return x.memory
  }
  prefix func * <T> (x: UnsafeMutablePointer<T>) -> inout T {
    return x.memory
  }

As we'll see in a later section, this can also be used as a building block for
more interesting property abstractions.

Semantics
---------

Inout returns should behave like other lvalues, and should maintain a scoped
discipline, like other inouts. If applied in a non-lvalue context, the
returned inout reference is immediately loaded::

  var x = 0
  let xRef: () -> inout Int = { &x }
  var y = xRef() // Loads a copy of x
  y = 1 // Does not mutate x
  xRef() = 2 // Does mutate x, doesn't mutate y

When a function takes an inout parameter and returns inout in an lvalue context,
the returned inout should be nested inside the parameter inout, as it may have
been projected from it::

  // The semantic order of operations:
  //  - get &building
  //  - set return value of firstResidentNameInBuilding
  //  - set &building
  firstResidentNameInBuilding(&building) = "John Jones"

Similarly, when an inout-returning function's return
expression involves inout subexpressions, the returned inout should arrange
itself to be scoped inside those inouts by passing a commit callback to the
caller (as already happens for logical ``materializeForSet`` operations)::

  func firstResidentNameInBuilding(inout b: [Int: Apartment])
  -> inout String {
    // Semantic order of operations:
    //  - get b
    //  - get first
    //  - force-unwrap
    //  - get resident
    //  - materializeForSet name
    //  - return materialized, with a commit callback
    // Commit callback:
    //  - set name
    //  - set resident
    //  - set first!
    //  - set first
    //  - set b
    return &b.first!.resident.name
  }

``inout`` would only be allowed to appear at the top level of a function's
return. The restrictions on inout make it difficult for a function
with heterogeneous inout and value returns to be useful. Likewise, the prefix
``&`` operator can only appear as the sole expression in a return statement,
or as the sole expression in a closure (in addition to its existing use in
parameter tuples).

``inout`` would still not be allowed as a generic substitution, so ``T -> inout
U`` is not substitutable for ``A -> B``. This means the compose operator needs
more overloads than it ought to, but I think that's an acceptable tradeoff.

Implementation
--------------

Implementation-wise, a function that returns ``inout`` is a lot like an object
with a custom subscript, albeit one we can implement a bit more efficiently,
since the ``get`` and ``set`` implementations are guaranteed to be related.
Invoking the function would do the equivalent of a subscript
``materializeForSet`` operation, returning a pointer to memory that
(at least temporarily) represents the property, and stashing the parameters
in a caller-provided side buffer. We also need a callback that commits the
final value and commits any dependent inouts within which the returned inout is
nested, along with an optional owner reference to keep alive any local
local variables or class references the returned inout is dependent on.

2. Contextual Instance Member Lookup
====================================

In addition to support for binding and projection, we also need more expressive
tools for building abstractions over properties and methods.  We want better
ways of representing Cocoa idioms like KVC and target-action that are currently
untyped and reliant on strings. Closures alone are inadequate for these and
other use cases--the memory management semantics of closures fight against
the established conventions for target-action and observation relationships,
and for observation, it's necessary to insert behavior
between steps of a projection along a key path, and it's not sufficient to
merely capture the projection in a single closure.

We have the contextual static member lookup syntax ``.foo``, which in a type
context ``T`` or ``T?`` looks up a static member of ``T`` with matching type
``T``. I think it would be natural to extend this syntax to apply in
function type context ``T -> U`` to do partially-applied instance
lookup. ``.foo`` would look in ``T`` for a property of type ``U`` and produce
its getter (or an ``inout`` reference to the property in ``T -> inout U``
context). Similarly, ``.bar(x)`` would produce a partial application of
``T.bar`` to its method argument ``x``, leaving ``self`` unbound::

  let matrix = [[1, 0, 0],
                [0, 1, 0],
                [0, 0, 1]]
  let columnCounts = matrix.map(.count) // Produces [3, 3, 3]
  let matrixPlusOne = matrix.map(.map { $0 + 1 })

  let adverbList: [String] -> String
    = .join(", ") ∘ .filter(.endsWith("ly"))

  adverbList(["adroitly", "hungry", "quickly"]) // ==> "adroitly, quickly"

This would be a big help in making the "object-oriented" and "functional"
realms of Swift interact better. OO programmers expect operations like
"map" and "filter" to be methods of the
collections they apply to, and functional programmers expect to be able to
partially apply them to their closure arguments. With this shorthand,
"map" and "filter" can be methods and also curry the right way to enable
expressive functional idioms. It also exposes properties
as functions in a natural way.

In situations where type context is insufficient, it should also be possible
to reference an unbound instance property relative to the type, to produce
a ``Self -> Property`` or ``Self -> inout Property`` projection::

  let arrayCount = [Int].count // returns .count as an [Int] -> Int
  arrayCount([1,2,3,4])        // ===> 4
  let upper = String.uppercase // String -> String
  upper("Floß")                // ===> "FLOSS"

The currying order of unbound instance methods should also be reversed for
consistency with the contextual syntax::

  let allPlusOne = [Int].map { $0 + 1 }
  allPlusOne([1, 2, 3])  // ===> [2, 3, 4]
  let adverbList = [String].join(", ") ∘ [String].filter(String.endsWith("ly"))
  adverbList(["slowly", "angry", "clumsily"]) // ===> "slowly, clumsily"

(Partially applying the ``self`` parameter first would still be possible by
``instance.method`` style partial application.)

3. @thin and @static Function Values
====================================

``@thin`` function values, that is, functions that don't capture any local
context, are a useful building block for presenting a typed, functional
interface in situations where the default capture semantics of closures are
undesirable.  For instance, the target-action idiom requires that the target be
``unowned`` by the responder in order to avoid introducing leaks in cyclic
responder graphs. By using contextual ``.foo`` syntax to produce ``@thin``
function values referencing method or property invocations, we can build an
expressive and syntactically lightweight abstraction that constrains the memory
management semantics of target-action references::

  // Build a target-action pair from a target and an action function. The
  // function must not capture any local context. The target is not owned
  // by the TargetAction value.
  func => <T: class> (target: T, action: @thin T -> ()) -> TargetAction {
    return TargetAction(target, action)
  }

  public struct TargetAction {
    private let invocation: () -> ()

    public init<T: class>(_ target: T, _ action: @thin T -> ()) {
      invocation = {[unowned target] in action(target) }
    }

    public func invoke() {
      invocation()
    }
  }

Even with the constraint that the action function must not capture context,
this adds a lot of expressivity over the traditional selector-based
target-action idiom, while preserving type safety and correct memory management
semantics::

  // Apply nullary methods...
  button1.setTarget(responder=>.cut())
  // ...or methods with specific arguments...
  button2.setTarget(button2=>.setTitle("Click me again, I dare you"))
  // ...or free functions...
  button3.setTarget(message=>print)
  // ...or (context-free) blocks of code!
  button4.setTarget(message => {
    print("The button so nice, it prints twice: \($0) \($0)")
  })

A further refinement on ``@thin`` would be to have ``@static`` function values,
which must be resolvable at compile-time to a specific function, method, or
thin closure.  Since static function references require no runtime context at
all, the compiler can do some magical things with them for C and Objective-C
interop, including:

- convert them to C function pointers, for interop with C APIs, or
- emit an Objective-C selector name and method implementation that invokes the
  function, for interop with selector-based ObjC APIs.

In particular, being able to selectorize static function references lets us
avoid designing a general "namespaced selector" feature; the compiler can
transparently produce selectors on demand only for code that needs them.
This would let us implement the ``TargetAction`` example above in an
Objective-C-compatible way::

  public struct TargetAction {
    public unowned let target: AnyObject
    public let action: Selector

    // Methods that take @static function references would have to be
    // transparent, so that they get instantiated with a static reference to
    // the function in question.
    @transparent
    public init<T: class>(_ target: T, _ action: @static T -> ()) {
      self.target = target
      // func Builtin.selectorize<T: class, U>(@static T -> U) -> Selector
      //   Tells the compiler to emit an implicit Objective-C method on
      //   the class T that has the effect of calling the given function.
      //   Evaluates to the magic selector generated to name the method.
      self.action = Builtin.selectorize(action)
    }

    public func invoke() {
      // It's safe to use ``performSelector:`` here because we know we
      // initialized the selector with a zero-argument function of our
      // target object.
      target._performSelector(action)
    }
  }

``@static`` function types have to be pretty limited--they could only
appear as local ``let`` bindings or as parameters of ``@transparent`` or
privileged builtin functions, and could only be bound to literal function
references or context-free closure literals. In any conditional or mutable
context they would immediately decay to ``@thin`` functions.

4. Examples
===========

These three features together give us a great substrate for building new
abstractions over property and method invocations. We've already seen how
target-action pairs can be represented. As another simple example, it becomes
possible to write a custom operator very similar to the builtin optional
chaining operator ``?``::

  infix operator ¿ {}

  func ¿ <T, U> (optional: T?, chain: T -> U) -> U?

  let x: [Int]? = [1, 3, 2]
  let y: [Int]? = nil

  x¿.count // ==> x ¿ (.count) ==> Some(3)
  y¿.count //                  ==> nil

  x¿.sorted(<) // ===> Some([1,2,3])

This has the added power of letting you chain closures or free functions::

  x ¿ .sorted(<) ¿ print // prints "[1, 2, 3]"
  y ¿ .sorted(<) ¿ print // does nothing

  x ¿ .sorted(<) ¿ { print($0[0]) } // prints "1"

Key Paths
---------

It's also possible to build a "key path" abstraction, based from an (unowned)
object reference, from which property projections can be chained, with
an observation step at each link of the chain::

  keypath(object)[.foo][.bar][.bas].value = "some value"

  public func keypath<T: class>(base: T) -> KeyPath<T> {
    return KeyPathBase(base)
  }

  // Abstract base class for immutable key paths.
  // Concrete subclasses are implementation details.
  public class KeyPath<T> {
    // Get the value referenced by the keypath.
    public var value: T { fatalError("abstract") }

    // Derive a mutable keypath relative to this one.
    public subscript<U>(element: T -> inout U) -> MutableKeyPath<U> {
      fatalError("abstract")
    }
  }

  // Abstract base class for mutable key paths.
  // Concrete subclasses are implementation details.
  public class MutableKeyPath<T>: KeyPath<T> {
    // Access the value referenced by the keypath.
    override var value: T {
      get { fatalError("abstract") }
      set { fatalError("abstract") }
    }

    // Derive a mutable keypath relative to this one.
    public subscript<U>(element: inout T -> inout U) -> MutableKeyPath<U> {
      fatalError("abstract")
    }
  }

  // Internal subclass that represents a base keypath that simply references
  // a class instance. The keypath does not own the referenced class.
  class KeyPathBase<T: class>: KeyPath<T> {
    unowned let base: T
    init(_ base: T) {
      self.base = base
    }

    // Derive a key path from an element of this one.
    override subscript<U>(element: T -> inout U) -> MutableKeyPath<U> {
      return KeyPathNode<U>({ &element(self.base) })
    }

    override var value: T { return base }
  }

  // Internal subclass that represents a mutable property projected from
  // another keypath.
  class KeyPathNode<T>: MutableKeyPath<T> {
    let element: () -> inout T
    
    override subscript<U>(element: inout T -> inout U) -> MutableKeyPath<U> {
      return KeyPathNode<U>({ &element(&self.value) })
    }

    override subscript<U>(element: T -> inout U) -> MutableKeyPath<U> {
      return KeyPathNode<U>({ &element(self.value) })
    }

    override var value: T {
      get {
        // bookkeeping for get...
        return element()
      }
      set {
        // bookkeeping for willChange...
        element() = newValue
        // bookkeeping for didChange...
      }
    }
  }

Lenses
------

Looking toward the functional world, the Haskell community has developed
"lenses" as a powerful way of working with and transforming projections.
Lenses support functional composition, projection, and traversal of aggregates
and heterogeneous collections. Here's a great introductory blog post:
https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
The building blocks for a simple lens are a "get" and "update" function
for the projected element, which respectively project out the element, and
produce a copy of the aggregate with the element changed::

  class Lens<From, To> {
    // For a complete implementation, see
    //   https://github.com/typelift/swiftz/blob/master/swiftz/Lens.swift

    init(getter: From -> To, setter: (From, To) -> From)
  }

both of which can be derived from an ``inout`` projection function,
making it syntactically lightweight to derive ``Lens`` objects from ``.foo``
property references::

  extension Lens {
    convenience init(_ projection: inout From -> inout To) {
      let getter: From -> To = {(var x) in projection(&x) }
      let setter: (From, To) -> From = {(var x, y) in 
        projection(&x) = y
        return x
      }
      self.init(getter: getter, setter: setter)
    }
  }

  prefix func *<From, To>(projection: inout From -> inout To)
      -> Lens<From, To> {
    return Lens(projection)
  }

