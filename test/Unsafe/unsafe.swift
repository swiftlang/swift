// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature DisallowUnsafe

// Make sure everything compiles without error when unsafe code is allowed.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature AllowUnsafeAttribute %s

// -----------------------------------------------------------------------
// Witness matching
// -----------------------------------------------------------------------
protocol P {
  func f()
  @unsafe func g()
}

struct XP: P {
  @unsafe func f() { } // expected-error{{unsafe instance method 'f()' cannot satisfy safe requirement}}
  @unsafe func g() { }
}

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
  unowned(unsafe) var s2: Super // expected-error{{unowned(unsafe) involves unsafe code}}
}

// -----------------------------------------------------------------------
// Inheritance of @unsafe
// -----------------------------------------------------------------------
@unsafe class UnsafeSuper { // expected-note 2{{'UnsafeSuper' declared here}}
  func f() { } // expected-note{{'f()' declared here}}
};

class UnsafeSub: UnsafeSuper { } // expected-error{{reference to unsafe class 'UnsafeSuper'}}

// -----------------------------------------------------------------------
// Declaration references
// -----------------------------------------------------------------------
@unsafe func unsafeF() { } // expected-note{{'unsafeF()' declared here}}
@unsafe var unsafeVar: Int = 0 // expected-note{{'unsafeVar' declared here}}

@unsafe struct PointerType { } // expected-note{{'PointerType' declared here}}

func testMe(
  _ pointer: PointerType, // expected-error{{reference to unsafe struct 'PointerType'}}
  _ unsafeSuper: UnsafeSuper // expected-error{{reference to unsafe class 'UnsafeSuper'}}
) { 
  unsafeF() // expected-error{{call to unsafe global function 'unsafeF'}}
  _ = unsafeVar // expected-error{{reference to unsafe var 'unsafeVar'}}
  unsafeSuper.f() // expected-error{{call to unsafe instance method 'f'}}
}
