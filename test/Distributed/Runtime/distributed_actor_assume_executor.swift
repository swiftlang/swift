// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// rdar://106822386
// REQUIRES: rdar106822386

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

func checkAssumeLocalDistributedActor(actor: MainFriend) /* synchronous! */ -> String {
  assumeOnLocalDistributedActorExecutor(actor) { dist in
    print("gained access to: \(dist.isolatedProperty)")
    return dist.isolatedProperty
  }
}

func checkAssumeMainActor(actor: MainFriend) /* synchronous! */ {
  assumeOnMainActorExecutor {
    print("yay")
  }
}

@MainActor
func check(actor: MainFriend) {
  _ = checkAssumeLocalDistributedActor(actor: actor)
  checkAssumeMainActor(actor: actor)
}

distributed actor MainFriend {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  let isolatedProperty: String = "Hello there!"

  distributed func test(x: Int) async throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    return checkAssumeMainActor(actor: self)
  }

}

actor OtherMain {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    return MainActor.sharedUnownedExecutor
  }

  func checkAssumeLocalDistributedActor(actor: MainFriend) /* synchronous! */ {
    _ = assumeOnLocalDistributedActorExecutor(actor) { dist in
      print("gained access to: \(dist.isolatedProperty)")
      return dist.isolatedProperty
    }
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeLocalDistributedActorExecutor")

    let system = FakeRoundtripActorSystem()
    let distLocal = MainFriend(actorSystem: system)

    if #available(SwiftStdlib 5.9, *) {

      tests.test("assumeOnLocalDistributedActorExecutor: assume the main executor, inside the DistributedMainFriend local actor") {
        _ = checkAssumeLocalDistributedActor(actor: distLocal)
        try! await distLocal.test(x: 42)
      }

      tests.test("assumeOnLocalDistributedActorExecutor: assume same actor as the DistributedMainFriend") {
        await OtherMain().checkAssumeLocalDistributedActor(actor: distLocal)
        try! await distLocal.test(x: 42)
      }

      tests.test("assumeOnLocalDistributedActorExecutor: wrongly assume the same actor as the DistributedmainFriend") {
        await OtherMain().checkAssumeLocalDistributedActor(actor: distLocal)
      }

      tests.test("assumeOnLocalDistributedActorExecutor: on remote actor reference") {
        expectCrashLater(withMessage: "Cannot assume to be 'isolated MainFriend' since distributed actor 'a.MainFriend' is remote.")
        let remoteRef = try! MainFriend.resolve(id: distLocal.id, using: system)
        await OtherMain().checkAssumeLocalDistributedActor(actor: remoteRef)
      }


    }

    await runAllTestsAsync()
  }
}
