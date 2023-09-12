// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature SendNonSendable -disable-availability-checking -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

/// Classes are always non-sendable, so this is non-sendable
class NonSendableKlass {
  func asyncCall() async {}
}

actor Actor {
  var klass = NonSendableKlass()
  final var finalKlass = NonSendableKlass()
}

final actor FinalActor {
  var klass = NonSendableKlass()
}

/////////////////////////////
// MARK: Closure Formation //
/////////////////////////////

// This test makes sure that we can properly pattern match project_box.
func formClosureWithoutCrashing() {
  var c = NonSendableKlass() // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}
  let _ = { print(c) }
}
