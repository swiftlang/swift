// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func unsafeFunction() { }

@unsafe
struct UnsafeType { }

@safe(unchecked)
func f() {
  unsafeFunction()
}

@safe(unchecked, message: "I was careful")
func g() {
  unsafeFunction()
}

// expected-warning@+2{{global function 'h' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe [Unsafe]}}
@safe(unchecked, message: "I was careful")
func h(_: UnsafeType) { // expected-note{{reference to unsafe struct 'UnsafeType'}}
  unsafeFunction()
}

// expected-warning@+1 {{global function 'rethrowing' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
func rethrowing(body: (UnsafeType) throws -> Void) rethrows { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

class HasStatics {
  // expected-warning@+1{{static method 'f' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe [Unsafe]}}{{3-3=@unsafe }}
  static internal func f(_: UnsafeType) { } // expected-note{{reference to unsafe struct 'UnsafeType'}}

  
}

@unsafe
func unsafeInt() -> Int { 5 }

struct HasProperties {
  @safe(unchecked) var computed: Int {
    unsafeInt()
  }

  @unsafe var computedUnsafe: Int {
    unsafeInt()
  }

  @safe(unchecked) static var blah: Int = {
    unsafeInt()
  }()

  @unsafe static var blahUnsafe: Int = {
    unsafeInt()
  }()
}

// Parsing issues
@safe // expected-error{{expected '(' in 'safe' attribute}}
func bad1() { }

@safe() // expected-error{{'@safe' attribute must be written as '@safe(unchecked)'}}
func bad2() { }

@safe(blah) // expected-error{{'@safe' attribute must be written as '@safe(unchecked)'}}
func bad3() { }

@safe(5) // expected-error{{'@safe' attribute must be written as '@safe(unchecked)'}}
func bad4() { }

@safe(unchecked, blah) // expected-error{{unknown option 'blah' for attribute 'safe'}}
func bad5() { }

@safe(unchecked, message) // expected-error{{expected ':' after label 'message'}}
func bad6() { }

@safe(unchecked, message: "a\(b)") // expected-error{{message cannot be an interpolated string literal}}
func bad7() { }
