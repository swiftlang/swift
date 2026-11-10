// RUN: %target-swift-frontend -typecheck -swift-version 6 -target %target-swift-5.1-abi-triple -enable-experimental-feature NontransferableTypes -parse-as-library -verify %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_NontransferableTypes

struct NonTransferable: ~Transferable {
  func use() {}
  @concurrent func work() async {}
}

func runOnMainNonisolated(_: @MainActor @nontransferable () -> Void) {}

actor Storage {
  var storage = NonTransferable()

  init() {}

  init(nt: NonTransferable) {
    storage = nt // Ok
  }

  init(asyncNT nt: NonTransferable) async {
    storage = nt // Ok
    storage = NonTransferable() // Ok
  }

  func take(_: NonTransferable) {}

  func storeFresh() {
    storage = NonTransferable() // Ok
  }

  func storeLocal() {
    let nt = NonTransferable()
    storage = nt // Ok
  }

  func captureInClosure() {
    let nt = NonTransferable()
    let fn: @nontransferable () -> Void = { self.take(nt) } // Ok
    fn()
  }
}

func captureInMainActorClosure() async {
  let nt = NonTransferable()
  let fn = { @MainActor @nontransferable in nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a main actor-isolated closure}}
  await fn()
}

@MainActor func captureInConcurrentClosure() async {
  let nt = NonTransferable()
  let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a @concurrent closure}}
  await fn()
}

nonisolated(nonsending) func captureInConcurrentClosureFromNonsending() async {
  let nt = NonTransferable()
  let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a @concurrent closure}}
  await fn()
}

// A synchronous function runs on its caller's executor.
func captureInConcurrentClosureFromSynchronous() {
  let nt = NonTransferable()
  let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a @concurrent closure}}
  _ = fn
}

extension Storage {
  func captureInConcurrentClosure() async {
    let nt = NonTransferable()
    let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() }
    // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a @concurrent closure}}
    await fn()
  }
}

// A closure literal takes on the isolation of its contextual type.
func passClosureLiteralsToMainActor(nt: NonTransferable) {
  runOnMainNonisolated { nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a main actor-isolated closure}}
  runOnMainNonisolated { [nt] in nt.use() }
  // expected-error@-1 {{capture of 'nt' with non-'Transferable' type 'NonTransferable' in a main actor-isolated closure}}
}
