// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -enable-experimental-feature NontransferableTypes -parse-as-library -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_NontransferableTypes

// A 'Transferable' value can't reference a non-'Transferable' one, so it can be
// sent even if region-based isolation merged their regions.

struct NT: ~Transferable {
  func use() {}
}

class C {
  func use() {}
  func take(_: NT) {}
}

@MainActor func useOnMain(_: C) {}
@MainActor func takeSendingOnMain(_: sending C) {}

func sendMergedRegion() async {
  let nt = NT()
  let c = C()
  c.take(nt)
  await useOnMain(c) // Ok
}

@MainActor func sendMergedRegionToSendingParameter() {
  let nt = NT()
  let c = C()
  c.take(nt)
  takeSendingOnMain(c) // Ok
}

func returnMergedRegionAsSending() -> sending C {
  let c = C()
  let nt = NT()
  c.take(nt)
  return c // Ok
}

@MainActor func captureMergedRegionInConcurrentClosure() async {
  let nt = NT()
  let c = C()
  c.take(nt)
  let fn = { @concurrent () async in c.use() } // Ok
  await fn()
}

// Code that type checking accepts and which involves the region of a
// non-'Transferable' value.

struct MainActorClosureBox: ~Transferable {
  var f: @MainActor @nontransferable () -> Void
}

@concurrent func useConcurrently(_: NT) async {}
nonisolated(nonsending) func useNonsending(_: NT) async {}
@concurrent func makeConcurrently() async -> NT { NT() }
nonisolated(nonsending) func makeNonsending() async -> NT { NT() }

actor Storage {
  var storage = NT()

  init(nt: NT) {
    storage = nt // Ok
  }

  init(asyncNT nt: NT) async {
    storage = nt // Ok
    storage = NT() // Ok
  }

  func take(_: NT) {}

  func storeLocal() {
    let nt = NT()
    storage = nt // Ok
  }

  func captureInClosure() {
    let nt = NT()
    let fn: @nontransferable () -> Void = { self.take(nt) } // Ok
    fn()
  }
}

@MainActor func sendToNonsending(nt: NT) async {
  await useNonsending(nt) // Ok
  let fresh = NT()
  await useNonsending(fresh) // Ok
}

@concurrent func sendFromConcurrent(nt: NT) async {
  await useConcurrently(nt) // Ok
  let fresh = NT()
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
  let nt = NT()
  let fn: @nontransferable @concurrent () async -> Void = { nt.use() } // Ok
  await fn()
}

@concurrent func captureInConcurrentClosureFromConcurrent() async {
  let nt = NT()
  let fn: @nontransferable () async -> Void = { @concurrent () async in nt.use() } // Ok
  await fn()
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

func convertSynchronousToConcurrentInConcurrent() async {
  let nt = NT()
  let f: @nontransferable () -> Void = { nt.use() }
  let g: @nontransferable @concurrent () async -> Void = f // Ok
  await g()
}

func convertMainActorToConcurrent(
  f: @escaping @MainActor @nontransferable () -> Void
) async {
  let g: @nontransferable @concurrent () async -> Void = f // Ok
  await g()
}

@MainActor func convertConcurrentToNonsending(
  f: @escaping @nontransferable @concurrent () async -> Void
) async {
  let g: nonisolated(nonsending) @nontransferable () async -> Void = f // Ok
  await g()
}
