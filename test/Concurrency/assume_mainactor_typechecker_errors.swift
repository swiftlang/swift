// RUN: %target-swift-frontend -swift-version 6 -emit-sil -enable-experimental-feature UnspecifiedMeansMainActorIsolated %s -verify

// REQUIRES: asserts

// READ THIS! This test is meant to check the specific isolation when
// UnspecifiedMeansMainActorIsolated is enabled in combination with validating
// behavior around explicitly non-Sendable types that trigger type checker
// specific errors. Please do not put other types of tests in here.

// Fake Sendable Data
class SendableData : @unchecked Sendable {}

nonisolated func getDataFromSocket() -> SendableData { SendableData() }

class Klass { // expected-note 3 {{}}
  let s = SendableData()

  init() { s = SendableData() }
  init(_ s: SendableData) {}

  func doSomething() {}
}

@available(*, unavailable)
extension Klass : Sendable {}

struct StructContainingKlass {
  var k = Klass()
}

func unspecifiedAsync<T>(_ t: T) async {}
nonisolated func nonisolatedAsync<T>(_ t: T) async {}
@MainActor func mainActorAsync<T>(_ t: T) async {}

func unspecifiedFunctionTest() async {
  let k = Klass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)
}

func unspecifiedFunctionTest2() async {
  let k = StructContainingKlass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)

  await unspecifiedAsync(k.k)
  await nonisolatedAsync(k.k)
  await mainActorAsync(k.k)
}

nonisolated func nonisolatedFunctionTest() async {
  let k = StructContainingKlass()
  await unspecifiedAsync(k.k) // expected-error {{non-sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  await nonisolatedAsync(k.k) // expected-error {{non-sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
  await mainActorAsync(k.k) // expected-error {{non-sendable type 'Klass' of property 'k' cannot exit main actor-isolated context}}
}

func testTask() async {
  Task {
    let k = Klass(getDataFromSocket())
    k.doSomething()
  }
}

func testTaskDetached() async {
  Task.detached {
    let k = Klass(getDataFromSocket())
    // Have to pop back onto the main thread to do something.
    await k.doSomething()
  }
}
