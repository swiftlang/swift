// Embedded Swift has no unspecialized generic code, so a generic method cannot
// be given a vtable entry: there is no single implementation to put there.
// Rather than reject every non-final generic method, they are dispatched
// statically and kept out of the vtable. The type checker makes that sound by
// rejecting the only two ways an override could exist:
//
//   * `open` -- a subclass in another module could override it, and we would
//     never see that subclass.
//   * `override` -- an override within this module would need the base method
//     to have a vtable entry to dispatch through.

// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -parse-stdlib -wmo

// REQUIRES: swift_feature_Embedded

// ---------------------------------------------------------------------------
// Accepted: nothing can override these.
// ---------------------------------------------------------------------------

public class Plain {
  public func generic<T>(_: T) { }
  internal func internalGeneric<T>(_: T) { }
  private func privateGeneric<T>(_: T) { }
  public final func finalGeneric<T>(_: T) { }
  public class func staticGeneric<T>(_: T) { }
  public func nonGeneric() { }
}

// An `open` class may still have `public` generic methods: `public` is not
// overridable from another module.
open class OpenClass {
  public func generic<T>(_: T) { }
  open func nonGeneric() { }
}

public final class FinalClass {
  public func generic<T>(_: T) { }
}

// A generic method whose genericity comes only from the class is not itself
// generic, so it keeps its vtable entry and can be overridden.
public class GenericClass<T> {
  public func usesClassParam(_: T) { }
}
public class GenericSub<T>: GenericClass<T> {
  public override func usesClassParam(_: T) { }
}

// ---------------------------------------------------------------------------
// Rejected: `open` generic methods.
// ---------------------------------------------------------------------------

open class HasOpenGenerics {
  open func openGeneric<T>(_: T) { }
  // expected-error@-1{{generic instance method 'openGeneric' in a class cannot be 'open' in Embedded Swift}}

  open class func openStaticGeneric<T>(_: T) { }
  // expected-error@-1{{generic class method 'openStaticGeneric' in a class cannot be 'open' in Embedded Swift}}
}

// `final` overrides the openness, so this is fine.
open class OpenButFinalMember {
  public final func f<T>(_: T) { }
}

// ---------------------------------------------------------------------------
// Rejected: overrides of generic methods.
// ---------------------------------------------------------------------------

public class Base {
  public func generic<T>(_: T) { }
  public func nonGeneric() { }
}

public class Derived: Base {
  public override func generic<T>(_: T) { }
  // expected-error@-1{{generic instance method 'generic' in a class cannot override another method in Embedded Swift}}

  // Overriding a non-generic method is unaffected.
  public override func nonGeneric() { }
}

// The error is on the override, not the base, so a base with no override is
// still accepted (see `Plain` above). An override two levels down is also
// caught.
public class Middle: Base { }
public class Bottom: Middle {
  public override func generic<T>(_: T) { }
  // expected-error@-1{{generic instance method 'generic' in a class cannot override another method in Embedded Swift}}
}

// A `final` override is still an override -- the base would need the vtable
// entry regardless.
public class FinalOverride: Base {
  public final override func generic<T>(_: T) { }
  // expected-error@-1{{generic instance method 'generic' in a class cannot override another method in Embedded Swift}}
}

// ---------------------------------------------------------------------------
// `required` generic initializers are a separate rule: they are reached through
// a dynamic type's metatype, so they cannot be dispatched statically at all.
// ---------------------------------------------------------------------------

public class Inits {
  public init<T>(value: T) { }        // okay, directly called
  public required init() { }          // okay, non-generic
  public required init<T>(other: T) { }
  // expected-warning@-1{{generic initializer 'init(other:)' in a class cannot be 'required' in Embedded Swift}}
}
