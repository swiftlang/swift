// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

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

@MainActor func transferToMain<T>(_ t: T) async {}
func useValueAsync<T>(_ t: T) async {}
func useValueSync<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

func callNonIsolatedAsync_TransferAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await transferToMain(x)
}

func callNonIsolatedSync_TransferAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await transferToMain(x)
}

func callNonIsolatedAsync_AsyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  await useValueAsync(x)
}

func callNonIsolatedSync_AsyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  await useValueAsync(x)
}

func callNonIsolatedAsync_SyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  await a.asyncNonisolated(x)
  await a.asyncNonisolated(x)
  useValueSync(x)
}

func callNonIsolatedSync_SyncUseAfter(_ a: MyActor) async {
  let x = NonSendable()
  a.syncNonisolated(x)
  a.syncNonisolated(x)
  useValueSync(x)
}
