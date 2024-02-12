// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature IsolatedAny -enable-upcoming-feature InferSendableFromCaptures %s

func globalNonisolatedFunction() {}
@MainActor func globalMainActorFunction() {}

actor A {
  func actorFunction() {}
}

func testBasic_sync() {
  // TODO: this is a pretty bad diagnostic
  let fn: @isolated(any) () -> () = globalNonisolatedFunction // expected-note {{calls to let 'fn' from outside of its actor context are implicitly asynchronous}}
  fn() // expected-error {{call to @isolated(any) let 'fn' in a synchronous nonisolated context}}
}

func testBasic_async_noawait() async {
  // TODO: this is a pretty bad diagnostic
  let fn: @isolated(any) () -> () = globalNonisolatedFunction

  // expected-error @+2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note @+1 {{calls to let 'fn' from outside of its actor context are implicitly asynchronous}}
  fn()
}

func testBasic_async_await() async {
  let fn: @isolated(any) () -> () = globalNonisolatedFunction
  await fn()
}

func requireSendableIsolatedAny(_ fn: @Sendable @isolated(any) () -> ()) {}

func testEraseFromNonisolated() {
  requireSendableIsolatedAny(globalNonisolatedFunction)
}

func testEraseFromGlobalActor() {
  requireSendableIsolatedAny(globalMainActorFunction)
}

// Apparently, the only way to get something that Sema will consider
// an actor-instance-isolated `@Sendable` function value is with
// `@_inheritActorContext`.
func requireSendableIsolatedAnyInheritContext(@_inheritActorContext _ fn : @Sendable @isolated(any) () -> ()) {}

extension A {
  func testEraseFromActorFunction() {
    // This actually works even without @isolated(any), which is a bug.
    // But it should definitely work with @isolated(any).
    requireSendableIsolatedAnyInheritContext {
      self.actorFunction()
    }
  }
}

func hasIsolatedArgument(actor: isolated A) {}
func requireIsolatedAnyWithActorArgument(_ fn: @Sendable @isolated(any) (A) -> ()) {}
func testEraseFromIsolatedArgument() {
  // expected-error @+1 {{cannot convert value of type '@Sendable (isolated A) -> ()' to expected argument type '@isolated(any) @Sendable (A) -> ()'}}
  requireIsolatedAnyWithActorArgument(hasIsolatedArgument)
}

func requireSendableNonIsolated(_ fn: @Sendable () -> ()) {}
func testConvertIsolatedAnyToNonIsolated(fn: @Sendable @isolated(any) () -> ()) {
  requireSendableNonIsolated(fn)
}

func requireSendableGlobalActor(_ fn: @Sendable @MainActor () -> ()) {}
func testConvertIsolatedAnyToMainActor(fn: @Sendable @isolated(any) () -> ()) {
  // expected-error @+1 {{cannot convert value of type '@isolated(any) @Sendable () -> ()' to expected argument type '@MainActor @Sendable () -> ()'}}
  requireSendableGlobalActor(fn)
}
