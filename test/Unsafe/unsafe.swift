// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift -enable-experimental-feature AllowUnsafeAttribute

// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %t -print-diagnostic-groups

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute %s -I %t

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

import unsafe_swift_decls

// -----------------------------------------------------------------------
// Witness matching
// -----------------------------------------------------------------------
protocol P {
  func f()
  @unsafe func g()
}

struct XP: P { // expected-warning{{conformance of 'XP' to protocol 'P' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe [Unsafe]}}{{12-12=@unsafe }}
  @unsafe func f() { } // expected-note{{unsafe instance method 'f()' cannot satisfy safe requirement}}
  @unsafe func g() { }
}

// -----------------------------------------------------------------------
// Conformances
// -----------------------------------------------------------------------

protocol Ptrable2 {
  associatedtype Ptr
}

extension HasAPointerType: Ptrable2 { } // expected-note{{unsafe type 'HasAPointerType.Ptr' (aka 'PointerType') cannot satisfy safe associated type 'Ptr'}}
  // expected-warning@-1{{conformance of 'HasAPointerType' to protocol 'Ptrable2' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe [Unsafe]}}{{28-28=@unsafe }}

struct UnsafeXP: @unsafe P {
  @unsafe func f() { }
  @unsafe func g() { }
}

protocol MultiP {
  associatedtype Ptr
  func f() -> Ptr
}

struct ConformsToMultiP { }

// expected-warning@+1{{conformance of 'ConformsToMultiP' to protocol 'MultiP' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe [Unsafe]}}{{29-29=@unsafe }}
extension ConformsToMultiP: MultiP {
  // expected-note@-1{{unsafe type 'UnsafeSuper' cannot satisfy safe associated type 'Ptr'}}
  @unsafe func f() -> UnsafeSuper {
    .init() // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    // expected-note@-1{{reference to unsafe initializer 'init()'}}
  }
}

protocol GenericP {
  associatedtype Ptr

  func f<T>(_: T, _: Ptr)
}

// expected-warning@+1{{conformance of 'ConformsToGenericP' to protocol 'GenericP' involves unsafe code; use '@unsafe' to indicate that the conformance is not memory-safe}}
struct ConformsToGenericP: GenericP {
  typealias Ptr = Int
  @unsafe func f<T>(_: T, _: Ptr) { } // expected-note{{unsafe instance method 'f' cannot satisfy safe requirement}}
}

// -----------------------------------------------------------------------
// Overrides
// -----------------------------------------------------------------------
class Super {
  func f() { } // expected-note{{overridden declaration is here}}
  @unsafe func g() { }
}

class Sub: Super { // expected-note{{make class 'Sub' @unsafe to allow unsafe overrides of safe superclass methods}}{{1-1=@unsafe }}
  @unsafe override func f() { } // expected-warning{{override of safe instance method with unsafe instance method [Unsafe]}}
  @unsafe override func g() { }  
}

// -----------------------------------------------------------------------
// Owned pointers
// -----------------------------------------------------------------------
struct SuperHolder {
  unowned var s1: Super
  unowned(unsafe) var s2: Super

  func getSuper2() -> Super {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{12-12=unsafe }}
    return s2 // expected-note{{reference to unowned(unsafe) property 's2' is unsafe}}
  }

  func getSuper2b() -> Super {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}
    s2 // expected-note{{reference to unowned(unsafe) property 's2' is unsafe}}
  }
}

// -----------------------------------------------------------------------
// Dynamic exclusivity check suppression
// -----------------------------------------------------------------------
class ExclusivityChecking {
  @exclusivity(unchecked) var value: Int = 0

  func next() -> Int {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}
    value += 1 // expected-note{{reference to @exclusivity(unchecked) property 'value' is unsafe}}
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{12-12=unsafe }}
    return value  // expected-note{{reference to @exclusivity(unchecked) property 'value' is unsafe}}
  }
}

// -----------------------------------------------------------------------
// Inheritance of @unsafe
// -----------------------------------------------------------------------
@unsafe class UnsafeSuper {
  func f() { }
};

// TODO: diagnose the need for @unsafe when there's an unsafe superclass.
class UnsafeSub: UnsafeSuper { }

// -----------------------------------------------------------------------
// Miscellaneous expression issues
// -----------------------------------------------------------------------

struct BufferThingy<T> {
  @unsafe init(count: Int) { }
}

func testConstruction() {
  let _ = BufferThingy<Int>(count: 17) // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe' [Unsafe]}}{{11-11=unsafe }}
  // expected-note@-1{{reference to unsafe initializer 'init(count:)'}}
}

func testRHS(b: Bool, x: Int) {
  @unsafe let limit = 17
  if b && x > limit { // expected-warning{{expression uses unsafe constructs but is not marked with 'unsafe' [Unsafe]}}{{6-6=unsafe }}
    // expected-note@-1{{reference to unsafe let 'limit'}}
  }
}

// -----------------------------------------------------------------------
// Declaration references
// -----------------------------------------------------------------------
@unsafe func unsafeF() { }
@unsafe var unsafeVar: Int = 0


// expected-warning@+3{{global function 'testMe' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func testMe(
  _ pointer: PointerType, // expected-note{{reference to unsafe struct 'PointerType'}}
  _ unsafeSuper: UnsafeSuper // expected-note{{reference to unsafe class 'UnsafeSuper'}}
) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  unsafeF() // expected-note{{reference to unsafe global function 'unsafeF()'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{7-7=unsafe }}
  _ = unsafeVar // expected-note{{reference to unsafe var 'unsafeVar'}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  unsafeSuper.f() // expected-note{{reference to unsafe instance method 'f()'}}
  // expected-note@-1{{reference to parameter 'unsafeSuper' involves unsafe type 'UnsafeSuper'}}

  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{7-7=unsafe }}
  _ = getPointers() // expected-note{{reference to global function 'getPointers()' involves unsafe type 'PointerType'}}
}

// -----------------------------------------------------------------------
// Various declaration kinds
// -----------------------------------------------------------------------

// expected-warning@+3{{type alias 'SuperUnsafe' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
typealias SuperUnsafe = UnsafeSuper // expected-note{{reference to unsafe class 'UnsafeSuper'}}

@unsafe typealias SuperUnsafe2 = UnsafeSuper

enum HasUnsafeThings {

// expected-warning@+3{{enum case 'one' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
case one(UnsafeSuper)  // expected-note{{reference to unsafe class 'UnsafeSuper'}}

@unsafe case two(UnsafeSuper)
}
