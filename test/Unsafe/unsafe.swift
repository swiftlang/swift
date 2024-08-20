// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift -enable-experimental-feature AllowUnsafeAttribute

// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %t

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute %s -I %t

import unsafe_swift_decls

// -----------------------------------------------------------------------
// Witness matching
// -----------------------------------------------------------------------
protocol P {
  func f()
  @unsafe func g()
}

struct XP: P {
  @unsafe func f() { } // expected-warning{{unsafe instance method 'f()' cannot satisfy safe requirement}}
  @unsafe func g() { }
}

// -----------------------------------------------------------------------
// Conformances
// -----------------------------------------------------------------------

protocol Ptrable2 {
  associatedtype Ptr // expected-note{{'Ptr' declared here}}
}

extension HasAPointerType: Ptrable2 { } // expected-warning{{unsafe type 'HasAPointerType.Ptr' (aka 'PointerType') cannot satisfy safe associated type 'Ptr'}}

// -----------------------------------------------------------------------
// Overrides
// -----------------------------------------------------------------------
class Super {
  func f() { }
  @unsafe func g() { }
}

class Sub: Super {
  @unsafe override func f() { }
  @unsafe override func g() { }  
}

// -----------------------------------------------------------------------
// Owned pointers
// -----------------------------------------------------------------------
struct SuperHolder {
  unowned var s1: Super
  unowned(unsafe) var s2: Super // expected-warning{{unowned(unsafe) involves unsafe code}}
}

// -----------------------------------------------------------------------
// Inheritance of @unsafe
// -----------------------------------------------------------------------
@unsafe class UnsafeSuper { // expected-note 3{{'UnsafeSuper' declared here}}
  func f() { } // expected-note{{'f()' declared here}}
};

class UnsafeSub: UnsafeSuper { } // expected-warning{{reference to unsafe class 'UnsafeSuper'}}

// -----------------------------------------------------------------------
// Declaration references
// -----------------------------------------------------------------------
@unsafe func unsafeF() { } // expected-note{{'unsafeF()' declared here}}
@unsafe var unsafeVar: Int = 0 // expected-note{{'unsafeVar' declared here}}

func testMe(
  _ pointer: PointerType, // expected-warning{{reference to unsafe struct 'PointerType'}}
  _ unsafeSuper: UnsafeSuper // expected-warning{{reference to unsafe class 'UnsafeSuper'}}
  // expected-note@-1{{'unsafeSuper' declared here}}
) { 
  unsafeF() // expected-warning{{call to unsafe global function 'unsafeF'}}
  _ = unsafeVar // expected-warning{{reference to unsafe var 'unsafeVar'}}
  unsafeSuper.f() // expected-warning{{call to unsafe instance method 'f'}}
  // expected-warning@-1{{reference to parameter 'unsafeSuper' involves unsafe type 'UnsafeSuper'}}

  _ = getPointers() // expected-warning{{call to global function 'getPointers' involves unsafe type 'PointerType'}}
}
