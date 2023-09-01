// RUN: %target-swift-frontend -warn-concurrency -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -warn-concurrency -parse-as-library %s -emit-sil -o /dev/null -verify -enable-experimental-feature SendNonSendable

// REQUIRES: concurrency
// REQUIRES: asserts

class GlobalCounter {
  var counter: Int = 0
}

let rs = GlobalCounter()
var globalInt = 17 // expected-note 2{{var declared here}}

class MyError: Error { // expected-warning{{non-final class 'MyError' cannot conform to 'Sendable'; use '@unchecked Sendable'}}
  var storage = 0 // expected-warning{{stored property 'storage' of 'Sendable'-conforming class 'MyError' is mutable}}
}

func testWarnings() {
  _ = rs // TODO: warn here
  _ = globalInt // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
  globalInt += 1 // expected-warning{{reference to var 'globalInt' is not concurrency-safe because it involves shared mutable state}}
}
