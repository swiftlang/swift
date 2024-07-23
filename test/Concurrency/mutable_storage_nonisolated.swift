// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts

class NonSendable {}

struct ImplicitlySendable {
  var x: Int
  nonisolated var y: Int // okay
}

struct ImplicitlyNonSendable {
  let x: NonSendable
  // expected-note@+1 {{convert 'y' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}
  nonisolated var y: Int // expected-error {{'nonisolated' cannot be applied to mutable stored properties}}
}

public struct PublicSendable: Sendable {
  nonisolated var x: Int // okay
}

public struct PublicNonSendable {
  // expected-note@+1 {{convert 'x' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}
  nonisolated var x: Int // expected-error {{'nonisolated' cannot be applied to mutable stored properties}}
}
