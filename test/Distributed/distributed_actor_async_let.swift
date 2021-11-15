// RUN: %target-typecheck-verify-swift -enable-experimental-distributed -disable-availability-checking

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

distributed actor Philosopher {
  let philosophy: String

  typealias Transport = AnyActorTransport

  init(transport: AnyActorTransport) {
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
