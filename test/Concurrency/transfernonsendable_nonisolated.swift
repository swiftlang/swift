// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

// This test validates behavior of transfernonsendable around nonisolated
// functions and fields.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendable {}

actor MyActor {
  nonisolated func asyncNonisolated(_ x: NonSendable) async {}
  nonisolated func syncNonisolated(_ x: NonSendable) {}
  nonisolated func asyncNonisolatedGeneric<T>(_ t: T) async {}
  nonisolated func syncNonisolatedGeneric<T>(_ t: T) {}
}

final actor MyFinalActor {
  nonisolated func asyncNonisolated(_ x: NonSendable) async {}
  nonisolated func syncNonisolated(_ x: NonSendable) {}
  nonisolated func asyncNonisolatedGeneric<T>(_ t: T) async {}
  nonisolated func syncNonisolatedGeneric<T>(_ t: T) {}
}

@MainActor func transferToMain<T>(_ t: T) async {}
func useValueAsync<T>(_ t: T) async {}
func useValueSync<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

func callMyActor_NonIsolatedAsync_TransferAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await transferToMain(x)
}

func callMyActor_NonIsolatedSync_TransferAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await transferToMain(x)
}

func callMyActor_NonIsolatedAsync_AsyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await useValueAsync(x)
}

func callMyActor_NonIsolatedSync_AsyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await useValueAsync(x)
}

func callMyActor_NonIsolatedAsync_SyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  useValueSync(x)
}

func callMyActor_NonIsolatedSync_SyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  useValueSync(x)
}

func callMyFinalActor_NonIsolatedAsync_TransferAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await transferToMain(x)
}

func callMyFinalActor_NonIsolatedSync_TransferAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await transferToMain(x)
}

func callMyFinalActor_NonIsolatedAsync_AsyncUseAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await useValueAsync(x)
}

func callMyFinalActor_NonIsolatedSync_AsyncUseAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await useValueAsync(x)
}

func callMyFinalActor_NonIsolatedAsync_SyncUseAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  useValueSync(x)
}

func callMyFinalActor_NonIsolatedSync_SyncUseAfter(_ a: MyFinalActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  useValueSync(x)
}

func callIsolatedFunction() async {
  let x = NonSendable()
  await useValueAsync(x)
  useValueSync(x)
}

@MainActor
func callMainActorIsolatedFunction() async {
  let x = NonSendable()
  await useValueAsync(x)
  useValueSync(x)
}
