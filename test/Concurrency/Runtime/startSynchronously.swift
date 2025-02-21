// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -swift-version 6 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest
@_spi(MainActorUtilities) import _Concurrency

actor SomeActor {
}

@globalActor
actor MyGlobalActor {
  static let shared: MyGlobalActor = MyGlobalActor()
}

// TODO: use better synchronization here rather than just rely on timing

func syncOnMyGlobalActor() -> [Task<Void, Never>] {
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  print("Confirmed to be on @MyGlobalActor")

  // This task must be guaranteed to happen AFTER 'tt' because we are already on this actor
  // so this enqueue must happen after we give up the actor.
  print("Step: schedule Task { @MyGlobalActor }, before startSynchronously \(#function) @ \(#line)")
  let t1 = Task { @MyGlobalActor in
    print("Step: inside Task { @MyGlobalActor }, before startSynchronously \(#function) @ \(#line)")
  }

  print("Step: before startSynchronously \(#function) @ \(#line)")
  let tt = Task.startSynchronously { @MyGlobalActor in
    print("Step: inside startSynchronously \(#function) @ \(#line)")
    try! await Task.sleep(for: .seconds(1))
    print("Step: after sleep, inside startSynchronously \(#function) @ \(#line)")
  }

  // CHECK: Confirmed to be on @MyGlobalActor
  // CHECK: Step: schedule Task { @MyGlobalActor }, before startSynchronously syncOnMyGlobalActor()
  // CHECK: Step: before startSynchronously syncOnMyGlobalActor()
  // We immediately entered the startSynchronously task:
  // CHECK: Step: inside startSynchronously syncOnMyGlobalActor
  // Only now we hit a suspension inside the sync task, and allow the other task to run
  // CHECK: Step: inside Task { @MyGlobalActor }, before startSynchronously syncOnMyGlobalActor()

  return [t1, tt]
}

print("==== ------------------------------------------------------------------")
await Task { @MyGlobalActor in
  MyGlobalActor.shared.preconditionIsolated("Should be executing on the global actor here")
  let ts = syncOnMyGlobalActor()

  print("Waiting for tasks...")
  for t in ts {
    await t.value
  }
}.value
