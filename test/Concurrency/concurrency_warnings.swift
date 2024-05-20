// RUN: %target-swift-frontend -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -disable-region-based-isolation-with-strict-concurrency
// RUN: %target-swift-frontend -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency
// REQUIRES: asserts

class GlobalCounter { // expected-note{{class 'GlobalCounter' does not conform to the 'Sendable' protocol}}
  var counter: Int = 0
}

let rs = GlobalCounter() // expected-warning {{let 'rs' is not concurrency-safe because non-'Sendable' type 'GlobalCounter' may have shared mutable state; this is an error in the Swift 6 language mode}}
// expected-note@-1 {{restrict 'rs' to the main actor if it will only be accessed from the main thread}}
// expected-note@-2 {{unsafely mark 'rs' as concurrency-safe if all accesses are protected by an external synchronization mechanism}}


var globalInt = 17 // expected-warning {{var 'globalInt' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in the Swift 6 language mode}}
// expected-note@-1 {{restrict 'globalInt' to the main actor if it will only be accessed from the main thread}}
// expected-note@-2 2{{var declared here}}
// expected-note@-3 {{unsafely mark 'globalInt' as concurrency-safe if all accesses are protected by an external synchronization mechanism}}
// expected-note@-4 {{convert 'globalInt' to a 'let' constant to make the shared state immutable}}

class MyError: Error { // expected-warning{{non-final class 'MyError' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  var storage = 0 // expected-warning{{stored property 'storage' of 'Sendable'-conforming class 'MyError' is mutable}}
}

func testWarnings() {
  _ = rs
  _ = globalInt // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
  globalInt += 1 // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
}
