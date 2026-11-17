// RUN: %target-swift-emit-silgen -enable-experimental-feature OnceFunctions -enable-upcoming-feature NonisolatedNonsendingByDefault %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-experimental-feature OnceFunctions %s | %FileCheck %s --check-prefix=NO-NONSENDING
// RUN: %target-swift-emit-silgen -enable-experimental-feature OnceFunctions -enable-upcoming-feature NonisolatedNonsendingByDefault -default-isolation MainActor %s | %FileCheck %s --check-prefix=MAINACTOR

// REQUIRES: swift_feature_OnceFunctions
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// The body closure of withoutActuallyDeiniting(_:do:) gets the same isolation
// as the body closure of withoutActuallyEscaping(_:do:).

func testWithoutActuallyDeiniting(
  _ f: consuming @once () async -> (),
  _ f2: consuming @escaping @once (Int) async -> Void
) async {
  // CHECK-LABEL: // closure #1 in testWithoutActuallyDeiniting(_:_:)
  // CHECK-NEXT: // Isolation: nonisolated(nonsending)
  // NO-NONSENDING-LABEL: // closure #1 in testWithoutActuallyDeiniting(_:_:)
  // NO-NONSENDING-NEXT: // Isolation: @concurrent
  // MAINACTOR-LABEL: // closure #1 in testWithoutActuallyDeiniting(_:_:)
  // MAINACTOR-NEXT: // Isolation: global_actor. type: MainActor
  await withoutActuallyDeiniting(f) {
    await $0()
  }

  // CHECK-LABEL: // closure #2 in testWithoutActuallyDeiniting(_:_:)
  // CHECK-NEXT: // Isolation: nonisolated(nonsending)
  // NO-NONSENDING-LABEL: // closure #2 in testWithoutActuallyDeiniting(_:_:)
  // NO-NONSENDING-NEXT: // Isolation: @concurrent
  // MAINACTOR-LABEL: // closure #2 in testWithoutActuallyDeiniting(_:_:)
  // MAINACTOR-NEXT: // Isolation: global_actor. type: MainActor
  await withoutActuallyDeiniting(f2) { body in
    await body(42)
  }
}

nonisolated func testExplicitlyNonisolated(
  _ f: consuming @once () async -> ()
) async {
  // CHECK-LABEL: // closure #1 in testExplicitlyNonisolated(_:)
  // CHECK-NEXT: // Isolation: nonisolated(nonsending)
  // NO-NONSENDING-LABEL: // closure #1 in testExplicitlyNonisolated(_:)
  // NO-NONSENDING-NEXT: // Isolation: @concurrent
  // MAINACTOR-LABEL: // closure #1 in testExplicitlyNonisolated(_:)
  // MAINACTOR-NEXT: // Isolation: nonisolated(nonsending)
  await withoutActuallyDeiniting(f) {
    await $0()
  }
}

func testExplicitGlobalActor(_ f: consuming @once () async -> ()) async {
  // CHECK-LABEL: // closure #1 in testExplicitGlobalActor(_:)
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // MAINACTOR-LABEL: // closure #1 in testExplicitGlobalActor(_:)
  // MAINACTOR-NEXT: // Isolation: global_actor. type: MainActor
  await withoutActuallyDeiniting(f) { @MainActor in
    await $0()
  }
}

func testSynchronousBody(_ f: consuming @once () -> ()) async {
  // A synchronous body is not affected.
  // CHECK-LABEL: // closure #1 in testSynchronousBody(_:)
  // CHECK-NEXT: // Isolation: nonisolated{{$}}
  // MAINACTOR-LABEL: // closure #1 in testSynchronousBody(_:)
  // MAINACTOR-NEXT: // Isolation: global_actor. type: MainActor
  withoutActuallyDeiniting(f) {
    $0()
  }
}

actor Test {
  // CHECK-LABEL: // closure #1 in Test.testActorIsolatedCapture(_:)
  // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
  // MAINACTOR-LABEL: // closure #1 in Test.testActorIsolatedCapture(_:)
  // MAINACTOR-NEXT: // Isolation: actor_instance. name: 'self'
  func testActorIsolatedCapture(_ f: consuming @once () async -> ()) async {
    await withoutActuallyDeiniting(f) {
      _ = self
      await $0()
    }
  }
}
