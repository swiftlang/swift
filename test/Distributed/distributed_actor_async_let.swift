// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
import FakeDistributedActorSystems

distributed actor Philosopher {
  typealias ActorSystem = FakeActorSystem

  let philosophy: String

  init(system: FakeActorSystem) {
    philosophy = "Epistemology"
  }

  distributed func hi() -> String { "Hi!" }

  func test(other: Philosopher) async throws {
    // self --------------------------------------------------------------------
    async let alet = self.hi() // none
    _ = await alet // Ok - `self.hi()` isn't throwing

    Task {
      _ = self.hi() // none
    }

    Task.detached {
      _ = await self.hi() // async
    }

    // other -------------------------------------------------------------------

    async let otherLet = other.hi() // hi = async throws because of `other`
    _ = try await otherLet

    Task {
      _ = try await other.hi() // hi = async throws
    }

    Task.detached {
      _ = try await other.hi() // hi = async throws
    }

    // known to be local -------------------------------------------------------

    // FIXME(distributed): relies on the "secretly known to be local" hack in typechecking
    let _: String? = await other.whenLocal { __secretlyKnownToBeLocal in
      // we're able to get state of what would otherwise be distributed-isolated:
      __secretlyKnownToBeLocal.philosophy
    }
  }

  static func test(iso: isolated Philosopher) async throws {
    _ = iso.hi() // we're "in the actor" already, since isolated

    // isolated parameter ------------------------------------------------------
    async let otherLet = iso.hi() // async
    _ = await otherLet

    Task {
      _ = await iso.hi() // none
    }

    Task.detached {
      _ = await iso.hi() // async
    }
  }
}
