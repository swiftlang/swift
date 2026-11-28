// RUN: %target-run-simple-swift(-enable-experimental-feature AllowIsolatedGlobalActorAttribute -Xfrontend -disable-availability-checking -swift-version 6 -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// TODO: Combine with 'distributed_actor_assume_isolated_custom_executors.swift' once we no longer need experimental flag
// REQUIRES: swift_feature_AllowIsolatedGlobalActorAttribute

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

@globalActor
actor MyGlobalActor {
  static let shared = MyGlobalActor()
}

@MyGlobalActor var counter = 0

func syncNonisolated() -> Int {
  MyGlobalActor.assumeIsolated {
    counter += 1
    return counter
  }
}

func syncGeneric<G: GlobalActor>(_: G.Type) -> String {
  G.assumeIsolated {
    "assumed \(G.self)"
  }
}

func hopToGeneric<G: GlobalActor>(
  _: G.Type, _ fn: @escaping @isolated(G) () -> Int
) async -> Int {
  await fn()
}

// Function type metadata for a generic global actor isolated function type
// is instantiated with the actual global actor
func erase<G: GlobalActor>(_: G.Type, _ fn: @escaping @isolated(G) () -> Int) -> Any {
  fn
}

@MyGlobalActor
func onMyGlobalActor() {
  // CHECK: counter = 1
  print("counter = \(syncNonisolated())")
  // CHECK: assumed MyGlobalActor
  print(syncGeneric(MyGlobalActor.self))
}

@main struct Main {
  static func main() async {
    await onMyGlobalActor()

    // CHECK: hopped = 2
    let hopped = await hopToGeneric(MyGlobalActor.self) {
      MyGlobalActor.shared.preconditionIsolated()
      counter += 1
      return counter
    }
    print("hopped = \(hopped)")

    let erased = erase(MyGlobalActor.self) { @MyGlobalActor in 7 }
    // CHECK: is MyGlobalActor: true
    print("is MyGlobalActor: \(erased is @MyGlobalActor () -> Int)")
    // The metadata records that the function is global actor isolated.
    // Note that dynamic casts don't compare which global actor it is
    // CHECK: is nonisolated: false
    print("is nonisolated: \(erased is () -> Int)")

    // CHECK: done
    print("done")
  }
}
