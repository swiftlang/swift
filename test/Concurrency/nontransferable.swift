// RUN: %target-swift-frontend -typecheck -swift-version 6 -target %target-swift-5.1-abi-triple -enable-experimental-feature NontransferableTypes -parse-as-library -verify %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_NontransferableTypes

struct NonTransferable: ~Transferable {
  func use() {}
  @concurrent func work() async {}
}

final class NonTransferableClass: ~Transferable {}

struct NoncopyableNonTransferable: ~Copyable, ~Transferable {}

struct MainActorClosureBox: ~Transferable {
  var f: @MainActor @nontransferable () -> Void
}

func useHere(_: NonTransferable) {}
@MainActor func useOnMain(_: NonTransferable) {}
@MainActor func useClassOnMain(_: NonTransferableClass) {}
@MainActor func useGenericOnMain<T: ~Transferable>(_: borrowing T) {}
@MainActor func consumeOnMain(_: consuming NoncopyableNonTransferable) {}
@MainActor func runOnMain(_: @nontransferable () -> Void) {}
@MainActor func runMainActorClosureOnMain(_: @MainActor @nontransferable () -> Void) {}
func runOnMainNonisolated(_: @MainActor @nontransferable () -> Void) {}
@MainActor func useBoxOnMain(_: MainActorClosureBox) {}
@preconcurrency @MainActor func preconcurrencyUseOnMain(_: NonTransferable) {}
@MainActor func makeOnMain() -> NonTransferable { NonTransferable() }
@concurrent func makeConcurrently() async -> NonTransferable { NonTransferable() }
nonisolated(nonsending) func makeNonsending() async -> NonTransferable { NonTransferable() }
@concurrent func useConcurrently(_: NonTransferable) async {}
nonisolated(nonsending) func useNonsending(_: NonTransferable) async {}

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

func sendParameter(nt: NonTransferable) async {
  await useOnMain(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into main actor-isolated context in call to global function 'useOnMain'}}
}

func sendFresh() async {
  let nt = NonTransferable()
  await useOnMain(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into main actor-isolated context in call to global function 'useOnMain'}}
}

func sendFreshTemporary() async {
  await useOnMain(NonTransferable())
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into main actor-isolated context in call to global function 'useOnMain'}}
}

func sendFreshClass() async {
  let c = NonTransferableClass()
  await useClassOnMain(c)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferableClass' cannot be sent into main actor-isolated context in call to global function 'useClassOnMain'}}
}

func sendFreshGeneric<T: ~Transferable>(_ make: () -> T) async {
  let t = make()
  await useGenericOnMain(t)
  // expected-error@-1 {{non-'Transferable' type 'T' cannot be sent into main actor-isolated context in call to global function 'useGenericOnMain'}}
}

func sendToActor(storage: Storage) async {
  let nt = NonTransferable()
  await storage.take(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into actor-isolated context in call to instance method 'take'}}
}

@MainActor func sendToActorInitializer() {
  let nt = NonTransferable()
  _ = Storage(nt: nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into actor-isolated context in call to initializer 'init(nt:)'}}
}

@MainActor func sendFromMainActor() async {
  let nt = NonTransferable()
  await useConcurrently(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot exit main actor-isolated context in call to nonisolated global function 'useConcurrently'}}
}

@MainActor func sendSelfFromMainActor() async {
  let nt = NonTransferable()
  await nt.work()
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot exit main actor-isolated context in call to nonisolated instance method 'work()'}}
}

func sendFunctionToMainActor() async {
  let nt = NonTransferable()
  let fn: @nontransferable () -> Void = { nt.use() } // Ok
  await runOnMain(fn)
  // expected-error@-1 {{non-'Transferable' type '@nontransferable () -> Void' cannot be sent into main actor-isolated context in call to global function 'runOnMain'}}
}

func sendConsumed() async {
  let x = NoncopyableNonTransferable()
  await consumeOnMain(x)
  // expected-error@-1 {{non-'Transferable' type 'NoncopyableNonTransferable' cannot be sent into main actor-isolated context in call to global function 'consumeOnMain'}}
}

func sendNonisolatedUnsafe() async {
  nonisolated(unsafe) let nt = NonTransferable()
  await useOnMain(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into main actor-isolated context in call to global function 'useOnMain'}}
}

func sendToPreconcurrency() async {
  let nt = NonTransferable()
  await preconcurrencyUseOnMain(nt)
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot be sent into main actor-isolated context in call to global function 'preconcurrencyUseOnMain'}}
}

// The region of 'fn' is main actor-isolated, but it's created in a @concurrent context.
func sendMainActorClosure() async {
  let fn = { @MainActor @nontransferable in }
  await runMainActorClosureOnMain(fn)
  // expected-error@-1 {{non-'Transferable' type '@MainActor @nontransferable () -> Void' cannot be sent into main actor-isolated context in call to global function 'runMainActorClosureOnMain'}}
}

func sendBoxedMainActorClosure() async {
  let box = MainActorClosureBox(f: { @MainActor in print("") })
  await useBoxOnMain(box)
  // expected-error@-1 {{non-'Transferable' type 'MainActorClosureBox' cannot be sent into main actor-isolated context in call to global function 'useBoxOnMain'}}
}

func returnFromMainActor() async {
  let nt = await makeOnMain()
  // expected-error@-1 {{non-'Transferable' result type 'NonTransferable' cannot be sent from main actor-isolated context in call to global function 'makeOnMain()'}}
  nt.use()
}

@MainActor func returnFromConcurrent() async {
  let nt = await makeConcurrently()
  // expected-error@-1 {{non-'Transferable' result type 'NonTransferable' cannot exit main actor-isolated context in call to nonisolated global function 'makeConcurrently()'}}
  nt.use()
}

// The callee isn't a declaration.
func callFunctionValues(
  _ fns: (use: @MainActor (NonTransferable) -> Void, make: @MainActor () -> NonTransferable)
) async {
  await fns.use(NonTransferable())
  // expected-error@-1 {{non-'Transferable' type 'NonTransferable' cannot cross from nonisolated to main actor-isolated context}}
  let nt = await fns.make()
  // expected-error@-1 {{non-'Transferable' result type 'NonTransferable' cannot cross from nonisolated to main actor-isolated context}}
  nt.use()
}

@MainActor func passClosureLiteralsToMainActorFromMainActor(nt: NonTransferable) {
  runOnMainNonisolated { nt.use() } // Ok
  runOnMainNonisolated { [nt] in nt.use() } // Ok
  runOnMainNonisolated { @nontransferable in nt.use() } // Ok
}

func useInPlace(nt: NonTransferable) async {
  useHere(nt) // Ok
  let fresh = NonTransferable()
  useHere(fresh) // Ok
}

@MainActor func sendWithinMainActor(nt: NonTransferable) async {
  useOnMain(nt) // Ok
  let fresh = NonTransferable()
  useOnMain(fresh) // Ok
  let fn: @nontransferable () -> Void = { fresh.use() }
  runOnMain(fn) // Ok
  let mainActorFn = { @MainActor @nontransferable in fresh.use() } // Ok
  mainActorFn()
}

@MainActor func sendToNonsending(nt: NonTransferable) async {
  await useNonsending(nt) // Ok
  let fresh = NonTransferable()
  await useNonsending(fresh) // Ok
}

@concurrent func sendFromConcurrent(nt: NonTransferable) async {
  await useConcurrently(nt) // Ok
  let fresh = NonTransferable()
  await useConcurrently(fresh) // Ok
}

@concurrent func returnFromConcurrentToConcurrent() async {
  let nt = await makeConcurrently() // Ok
  nt.use()
}

@MainActor func returnFromNonsending() async {
  let nt = await makeNonsending() // Ok
  nt.use()
}

// The closure is isolated to the main actor, like its context, so it hops to
// the main actor when called through the @concurrent type.
@MainActor func captureInClosureOfConcurrentType() async {
  let nt = NonTransferable()
  let fn: @nontransferable @concurrent () async -> Void = { nt.use() } // Ok
  await fn()
}

@concurrent func captureInConcurrentClosureFromConcurrent() async {
  let nt = NonTransferable()
  let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() } // Ok
  await fn()
}

@MainActor func callConcurrentFunctionParameter(
  fn: @nontransferable @concurrent () async -> Void
) async {
  await fn() // Ok
}

func formMainActorClosure() async {
  let fn = { @MainActor @nontransferable in print("") } // Ok
  await fn()
}

func assignMainActorClosures() async {
  var fn: @MainActor @nontransferable () -> Void = { @MainActor in print("") } // Ok
  fn = { @MainActor in print("1") } // Ok
  await fn()
}

func boxMainActorClosure() async {
  let box = MainActorClosureBox(f: { @MainActor in print("") }) // Ok
  await box.f()
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
