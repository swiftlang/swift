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

struct XP: P {
  // expected-note@-1{{make the enclosing struct @unsafe to allow unsafe conformance to protocol 'P'}}{{1-1=@unsafe }}
  @unsafe func f() { } // expected-warning{{unsafe instance method 'f()' cannot satisfy safe requirement [Unsafe]}}
  @unsafe func g() { }
}

// -----------------------------------------------------------------------
// Conformances
// -----------------------------------------------------------------------

protocol Ptrable2 {
  associatedtype Ptr // expected-note{{'Ptr' declared here}}
}

extension HasAPointerType: Ptrable2 { } // expected-warning{{unsafe type 'HasAPointerType.Ptr' (aka 'PointerType') cannot satisfy safe associated type 'Ptr'}}
  // expected-note@-1{{make the enclosing extension @unsafe to allow unsafe conformance to protocol 'Ptrable2'}}{{1-1=@unsafe }}

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

  // expected-warning@+1{{instance method 'getSuper2' involves unsafe code; use '@safe(unchecked)' to assert that the code is memory-safe}}
  func getSuper2() -> Super {
    return s2 // expected-note{{reference to unowned(unsafe) property 's2' is unsafe}}
  }

  // expected-warning@+1{{instance method 'getSuper2b' involves unsafe code; use '@safe(unchecked)' to assert that the code is memory-safe}}
  func getSuper2b() -> Super {
    s2 // expected-note{{reference to unowned(unsafe) property 's2' is unsafe}}
  }
}

// -----------------------------------------------------------------------
// Inheritance of @unsafe
// -----------------------------------------------------------------------
@unsafe class UnsafeSuper { // expected-note{{'UnsafeSuper' declared here}}
  func f() { }
};

class UnsafeSub: UnsafeSuper { } // expected-warning{{class 'UnsafeSub' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
// expected-note@-1{{reference to unsafe class 'UnsafeSuper'}}

// -----------------------------------------------------------------------
// Declaration references
// -----------------------------------------------------------------------
@unsafe func unsafeF() { }
@unsafe var unsafeVar: Int = 0

// expected-warning@+1{{global function 'testMe' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe [Unsafe]}}{{1-1=@unsafe }}
func testMe(
  _ pointer: PointerType, // expected-note{{reference to unsafe struct 'PointerType'}}
  _ unsafeSuper: UnsafeSuper // expected-note{{reference to unsafe class 'UnsafeSuper'}}
) { 
  unsafeF() // expected-note{{call to unsafe global function 'unsafeF()'}}
  _ = unsafeVar // expected-note{{reference to unsafe var 'unsafeVar'}}
  unsafeSuper.f() // expected-note{{call to unsafe instance method 'f()'}}
  // expected-note@-1{{reference to parameter 'unsafeSuper' involves unsafe type 'UnsafeSuper'}}

  _ = getPointers() // expected-note{{call to global function 'getPointers()' involves unsafe type 'PointerType'}}
}

// -----------------------------------------------------------------------
// Various declaration kinds
// -----------------------------------------------------------------------
// expected-warning@+1{{type alias 'SuperUnsafe' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
typealias SuperUnsafe = UnsafeSuper // expected-note{{reference to unsafe class 'UnsafeSuper'}}
@unsafe typealias SuperUnsafe2 = UnsafeSuper

enum HasUnsafeThings {
// expected-warning@+1{{enum case 'one' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
case one(UnsafeSuper) // expected-note{{reference to unsafe class 'UnsafeSuper'}}

@unsafe case two(UnsafeSuper)
}
