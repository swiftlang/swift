// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

distributed actor Philosopher {
  typealias ActorSystem = FakeActorSystem

  let philosophy: String

  init(system: FakeActorSystem) {
    philosophy = "Epistemology"
  }

  distributed func hi1() -> String { "Hi!" }
//  distributed func hi2() -> String { "Hi!" }
//  distributed func hi3() -> String { "Hi!" }
//  distributed func hi4() -> String { "Hi!" }
//  distributed func hi5() -> String { "Hi!" }
//  distributed func hi6() -> String { "Hi!" }
//  distributed func hi7() -> String { "Hi!" }
//  distributed func hi8() -> String { "Hi!" }
//  distributed func hi9() -> String { "Hi!" }

  func test(other: Philosopher) async throws {
    // self --------------------------------------------------------------------
    async let alet = self.hi1() // none
    _ = await alet // Ok - `self.hi()` isn't throwing

//    Task {
//      _ = self.hi1() // none
//    }
//
//    Task.detached {
//      _ = await self.hi2() // async
//    }
  }

//    // other -------------------------------------------------------------------
//
//    async let otherLet = other.hi3() // hi = async throws because of `other`
//    _ = try await otherLet
//
//    Task {
//      _ = try await other.hi4() // hi = async throws
//    }
//
//    Task.detached {
//      _ = try await other.hi5() // hi = async throws
//    }
//
//    // known to be local -------------------------------------------------------
//
//    // FIXME(distributed): relies on the "secretly known to be local" hack in typechecking
//    let _: String? = await other.whenLocal { __secretlyKnownToBeLocal in
//      // we're able to get state of what would otherwise be distributed-isolated:
//      __secretlyKnownToBeLocal.philosophy
//    }
//  }
//
//  static func test(iso: isolated Philosopher) async throws {
//    _ = iso.h6() // we're "in the actor" already, since isolated
//
//    // isolated parameter ------------------------------------------------------
//    async let otherLet = iso.hi7() // async
//    _ = await otherLet
//
//    Task {
//      _ = await iso.hi8() // none
//    }
//
//    Task.detached {
//      _ = await iso.hi9() // async
//    }
//  }
}
