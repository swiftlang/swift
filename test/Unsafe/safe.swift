// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -print-diagnostic-groups

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

@unsafe
func unsafeFunction() { }

@unsafe
struct UnsafeType { } // expected-note{{unsafe struct 'UnsafeType' declared here}}

@safe(unchecked)
func f() {
  unsafeFunction()
}

@safe(unchecked, message: "I was careful")
func g() {
  unsafeFunction()
}

// expected-note@+2{{make global function 'h' @unsafe to indicate that its use is not memory-safe}}
@safe(unchecked, message: "I was careful")
func h(_: UnsafeType) { // expected-warning{{reference to unsafe struct 'UnsafeType' [Unsafe]}}
  unsafeFunction()
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
