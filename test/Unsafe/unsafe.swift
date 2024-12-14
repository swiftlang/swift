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
  // expected-note@-1{{mark the enclosing struct with '@unsafe' to allow unsafe conformance to protocol 'P'}}{{1-1=@unsafe }}
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
  // expected-note@-1{{mark the enclosing extension with '@unsafe' to allow unsafe conformance to protocol 'Ptrable2'}}{{1-1=@unsafe }}

// -----------------------------------------------------------------------
// Overrides
// -----------------------------------------------------------------------
class Super {
  func f() { } // expected-note{{overridden declaration is here}}
  @unsafe func g() { }
}

class Sub: Super { // expected-note{{mark the class 'Sub' '@unsafe' to allow unsafe overrides of safe superclass methods}}{{1-1=@unsafe }}
  @unsafe override func f() { } // expected-warning{{override of safe instance method with unsafe instance method [Unsafe]}}
  @unsafe override func g() { }  
}

// -----------------------------------------------------------------------
// Owned pointers
// -----------------------------------------------------------------------
struct SuperHolder {
  unowned var s1: Super
  unowned(unsafe) var s2: Super // expected-warning{{unowned(unsafe) involves unsafe code}}
  // expected-note@-1{{mark the enclosing property 's2' '@unsafe' to allow it to use unsafe constructs}}{{3-3=@unsafe }}
}

// -----------------------------------------------------------------------
// Inheritance of @unsafe
// -----------------------------------------------------------------------
@unsafe class UnsafeSuper { // expected-note 3{{'UnsafeSuper' declared here}}
  func f() { } // expected-note{{unsafe instance method 'f' declared here}}
};

class UnsafeSub: UnsafeSuper { } // expected-warning{{reference to unsafe class 'UnsafeSuper'}}
// expected-note@-1{{mark the enclosing class 'UnsafeSub' '@unsafe' to allow it to use unsafe constructs}}{{1-1=@unsafe }}

// -----------------------------------------------------------------------
// Declaration references
// -----------------------------------------------------------------------
@unsafe func unsafeF() { } // expected-note{{unsafe global function 'unsafeF' declared here}}
@unsafe var unsafeVar: Int = 0 // expected-note{{'unsafeVar' declared here}}

// expected-note@+1 7{{mark the enclosing global function 'testMe' '@unsafe' to allow it to use unsafe constructs}}{{1-1=@unsafe }}
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
