// RUN: %target-run-simple-swift(-Onone -Xfrontend -disable-direct-retain-release -import-objc-header %S/Inputs/nontransferable_refcounting.h -enable-experimental-feature NontransferableTypes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_NontransferableTypes

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Values of non-'Transferable' types are copied and destroyed without atomic
// reference counting, including through the value witnesses that the runtime
// provides for function types. 'Transferable' values must still be counted
// atomically.

final class NT: ~Transferable {
  let id: Int
  init(_ id: Int) { self.id = id }
  deinit { print("deinit NT \(id)") }
}

@inline(never)
func duplicate<V: ~Transferable>(_ value: borrowing V) -> (V, V) {
  (copy value, copy value)
}

/// Reports whether the object that `body` allocates first is ever retained or
/// released atomically.
func refCounting(_ name: String, _ body: @nontransferable () -> Void) {
  watchNextAllocation()
  body()
  print("\(name): \(stopWatching() == 0 ? "non-atomic" : "atomic")")
}

func testClosures() {
  let id = 1
  let nt: @nontransferable () -> Void = { _ = id }
  let t: () -> Void = { _ = id }

  // The first allocation is the context of the reabstraction thunk, which is
  // copied by the value witnesses of the function type.
  refCounting("@nontransferable closure") { _ = duplicate(nt) }
  refCounting("closure") { _ = duplicate(t) }
}

installRefCountingHooks()

// CHECK: deinit NT 0
// CHECK-NEXT: NT: non-atomic
refCounting("NT") { _ = duplicate(NT(0)) }

// CHECK-NEXT: @nontransferable closure: non-atomic
// CHECK-NEXT: closure: atomic
testClosures()
