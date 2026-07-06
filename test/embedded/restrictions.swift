// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -verify-additional-prefix nonembedded-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -verify-additional-prefix embedded-
// RUN: %target-swift-frontend -typecheck %s -suppress-warnings -enable-experimental-feature Embedded -DSUPPRESS_WEAK
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// ---------------------------------------------------------------------------
// Untyped throws - we're not diagnosing this.
// ---------------------------------------------------------------------------

enum MyError: Error {
case failed
}

func untypedThrows() throws { }

func rethrowingFunction(param: () throws -> Void) rethrows { }

typealias FnType = () throws -> Void

func untypedThrowsInBody() {
  do throws {
    throw MyError.failed
  } catch {
  }

  _ = { (x) throws in x + 1 }
}

struct SomeStruct {
  init() throws { }

  var value: Int {
    get throws {
      0
    }
  }
}

// ---------------------------------------------------------------------------
// weak/unowned references
// ---------------------------------------------------------------------------

// Note: this have always been an error in Embedded Swift. Make sure they stay
// that way, but are emitted as warnings when the restrictions are enabled.

public class MyClass { }

public struct MyStruct {
  #if !SUPPRESS_WEAK
  var normalVar: MyClass
  weak var weakVar: MyClass? // expected-nonembedded-warning {{attribute 'weak' cannot be used in Embedded Swift}}
  // expected-embedded-error@-1 {{attribute 'weak' cannot be used in Embedded Swift}}

  unowned var unownedVar: MyClass // expected-nonembedded-warning {{attribute 'unowned' cannot be used in Embedded Swift}}
  // expected-embedded-error @-1{{attribute 'unowned' cannot be used in Embedded Swift}}
  #endif

  unowned(unsafe) var unownedUnsafe: MyClass
}

// ---------------------------------------------------------------------------
// generic, non-final functions
// ---------------------------------------------------------------------------

protocol P { }

class MyGenericClass<T> {
  func f<U>(value: U) { } // expected-warning{{generic instance method 'f(value:)' in a class must be 'final' in Embedded Swift}}
  func g() { }
  class func h() where T: P { } // expected-warning{{generic class method 'h()' in a class must be 'final' in Embedded Swift}}

  init<U>(value: U) { } // okay, can be directly called

  required init() { } // non-generic is okay

  required init<V>(something: V) { } // expected-warning{{generic initializer 'init(something:)' in a class cannot be 'required' in Embedded Swift}}
}

// ---------------------------------------------------------------------------
// generic functions on existentials
// ---------------------------------------------------------------------------

public protocol Q {
  func f<T>(_ value: T)
  func okay()
}

extension Q {
  public func g<T>(_ value: T) {
    f(value)
  }

  public mutating func h<T>(_ value: T) {
    f(value)
  }
}

public func existentials(q: any AnyObject & Q, i: Int) {
  q.okay()
  q.f(i) // expected-warning{{cannot use generic instance method 'f' on a value of type 'any AnyObject & Q' in Embedded Swift}}

  q.g(i) // expected-warning{{cannot use generic instance method 'g' on a value of type 'any AnyObject & Q' in Embedded Swift}}

  var qm = q
  qm.h(i) // expected-warning{{cannot use generic instance method 'h' on a value of type 'any AnyObject & Q' in Embedded Swift}}
}

// ---------------------------------------------------------------------------
// Dynamic casting restrictions
// ---------------------------------------------------------------------------

class ConformsToQ: Q {
  final func f<T>(_ value: T) { }
  func okay() { }
}

func dynamicCasting(object: AnyObject, cq: ConformsToQ) {
  // expected-warning@+1{{cannot perform a dynamic cast to a type involving protocol 'Q' in Embedded Swift}}
  if let q = object as? any AnyObject & Q {
    _ = q
  }

  // expected-warning@+1{{cannot perform a dynamic cast to a type involving protocol 'Q' in Embedded Swift}}
  if object is any AnyObject & Q { }

  // expected-warning@+1{{cannot perform a dynamic cast to a type involving protocol 'Q' in Embedded Swift}}
  _ = object as! AnyObject & Q

  _ = cq as AnyObject & Q
}

// ---------------------------------------------------------------------------
// #if handling to suppress diagnostics for non-Embedded-only code
// ---------------------------------------------------------------------------

#if $Embedded

func stillProblematic(object: AnyObject) {
  // expected-embedded-warning@+1{{cannot perform a dynamic cast to a type involving protocol 'Q' in Embedded Swift}}
  if let q = object as? any AnyObject & Q {
    _ = q
  }
}

#else

func notProblematicAtAll(object: AnyObject) throws {
  if let q = object as? any AnyObject & Q {
    _ = q
  }
}

#endif

#if !hasFeature(Embedded)
func stillNotProblematicAtAll(object: AnyObject) throws {
  if let q = object as? any AnyObject & Q {
    _ = q
  }
}
#endif
