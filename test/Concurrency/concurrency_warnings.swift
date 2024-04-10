// RUN: %target-swift-frontend -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -disable-region-based-isolation-with-strict-concurrency
// RUN: %target-swift-frontend -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency
// REQUIRES: asserts

class GlobalCounter {
  var counter: Int = 0
}

let rs = GlobalCounter() // expected-warning {{let 'rs' is not concurrency-safe because non-'Sendable' type 'GlobalCounter' may have shared mutable state; this is an error in the Swift 6 language mode}}
// expected-note@-1 {{isolate 'rs' to a global actor, or conform 'GlobalCounter' to 'Sendable'}}

var globalInt = 17 // expected-warning {{var 'globalInt' is not concurrency-safe because it is non-isolated global shared mutable state; this is an error in the Swift 6 language mode}}
// expected-note@-1 {{isolate 'globalInt' to a global actor, or convert it to a 'let' constant and conform it to 'Sendable'}}
// expected-note@-2 2{{var declared here}}


class MyError: Error { // expected-warning{{non-final class 'MyError' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  var storage = 0 // expected-warning{{stored property 'storage' of 'Sendable'-conforming class 'MyError' is mutable}}
}

func testWarnings() {
  _ = rs
  _ = globalInt // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
  globalInt += 1 // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
}
