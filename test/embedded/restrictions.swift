// RUN: %target-typecheck-verify-swift -Wwarning EmbeddedRestrictions -verify-additional-prefix nonembedded-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -verify-additional-prefix embedded-

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// ---------------------------------------------------------------------------
// Untyped throws
// ---------------------------------------------------------------------------

enum MyError: Error {
case failed
}

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{28-28=(<#thrown error type#>)}}
func untypedThrows() throws { }

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{41-41=(<#thrown error type#>)}}
func rethrowingFunction(param: () throws -> Void) rethrows { }

// expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{29-29=(<#thrown error type#>)}}
typealias FnType = () throws -> Void

func untypedThrowsInBody() {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{12-12=(<#thrown error type#>)}}
  do throws {
    throw MyError.failed
  } catch {
  }

  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{19-19=(<#thrown error type#>)}}
  _ = { (x) throws in x + 1 }
}

struct SomeStruct {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{16-16=(<#thrown error type#>)}}
  init() throws { }

  var value: Int {
  // expected-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{15-15=(<#thrown error type#>)}}
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
  var normalVar: MyClass
  weak var weakVar: MyClass? // expected-nonembedded-warning {{attribute 'weak' cannot be used in Embedded Swift}}
  // expected-embedded-error@-1 {{attribute 'weak' cannot be used in Embedded Swift}}

  unowned var unownedVar: MyClass // expected-nonembedded-warning {{attribute 'unowned' cannot be used in Embedded Swift}}
  // expected-embedded-error @-1{{attribute 'unowned' cannot be used in Embedded Swift}}

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

// expected-embedded-warning@+1{{untyped throws is not available in Embedded Swift; add a thrown error type with '(type)'}}{{31-31=(<#thrown error type#>)}}
func stillProblematic() throws { }

#else

func notProblematicAtAll() throws { }

#endif

#if !hasFeature(Embedded)
func stillNotProblematicAtAll() throws { }
#endif
