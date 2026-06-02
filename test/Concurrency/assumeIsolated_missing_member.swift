// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

class Foo {
  var i: Int // expected-note 2 {{'i' declared here}}
  init(i: Int) {
    self.i = i
  }
}

let foos = [Foo(i: 1), Foo(i: 2)]
MainActor.assumeIsolated {
  let _ = foos.sorted(by: { $0.f < $1.f })
  // expected-error@-1 2 {{value of type 'Foo' has no member 'f'; did you mean 'i'?}}
}
