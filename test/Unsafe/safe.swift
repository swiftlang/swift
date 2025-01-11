// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func unsafeFunction() { }

@unsafe
struct UnsafeType { }

func f() {
  unsafe unsafeFunction()
}

func g() {
  unsafe unsafeFunction()
}

// expected-warning@+1{{global function 'h' has an interface that is not memory-safe; use '@unsafe' to indicate that its use is unsafe}}
func h(_: UnsafeType) { // expected-note{{reference to unsafe struct 'UnsafeType'}}
// expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  unsafeFunction() // expected-note{{reference to unsafe global function 'unsafeFunction()'}}

  // okay
  unsafe unsafeFunction()

  // expected-warning@+1{{no unsafe operations occur within 'unsafe' expression}}
  unsafe g()
}

// expected-warning@+1 {{global function 'rethrowing' has an interface that is not memory-safe; use '@unsafe' to indicate that its use is unsafe}}{{1-1=@unsafe }}
func rethrowing(body: (UnsafeType) throws -> Void) rethrows { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

class HasStatics {
  // expected-warning@+1{{static method 'f' has an interface that is not memory-safe; use '@unsafe' to indicate that its use is unsafe }}{{3-3=@unsafe }}
  static internal func f(_: UnsafeType) { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

  
}

@unsafe
func unsafeInt() -> Int { 5 }

struct HasProperties {
  var computed: Int {
    unsafe unsafeInt()
  }

  @unsafe var computedUnsafe: Int {
    unsafe unsafeInt()
  }

  static var blah: Int = {
    unsafe unsafeInt()
  }()

  @unsafe static var blahUnsafe: Int = {
    unsafe unsafeInt()
  }()
}

// Parsing of `unsafe` expressions.
func testUnsafePositionError() -> Int {
  return 3 + unsafe unsafeInt() // expected-error{{'unsafe' cannot appear to the right of a non-assignment operator}}
}

