// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking %s

// expected-error @+1 {{non-async functions cannot inherit an executor}}
@_unsafeInheritExecutor
func testNonAsync() {}

@_unsafeInheritExecutor
func testAsync() async {}

struct A {
  // expected-error @+1 {{@_unsafeInheritExecutor may only be used on 'func' declarations}}
  @_unsafeInheritExecutor
  init() async {}

  // expected-error @+1 {{non-async functions cannot inherit an executor}}
  @_unsafeInheritExecutor
  func testNonAsync() {}

  @_unsafeInheritExecutor
  func testAsync() async {}
}


class NonSendableObject { // expected-note{{class 'NonSendableObject' does not conform to the 'Sendable' protocol}}
  var property = 0
}

@_unsafeInheritExecutor
func useNonSendable(object: NonSendableObject) async {}

actor MyActor {
  var object = NonSendableObject()
  func foo() async {
    // expected-warning@+1{{non-sendable type 'NonSendableObject' exiting actor-isolated context in call to non-isolated global function 'useNonSendable(object:)' cannot cross actor boundary}}
    await useNonSendable(object: self.object)
  }
}
