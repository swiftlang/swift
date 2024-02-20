import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@available(SwiftStdlib 5.7, *)
distributed actor FiveSevenActor_NothingExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  @available(SwiftStdlib 5.9, *)
  distributed func test(x: Int) throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}

@available(SwiftStdlib 5.9, *)
distributed actor FiveNineActor_NothingExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  distributed func test(x: Int) throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}

@available(SwiftStdlib 5.7, *)
distributed actor FiveSevenActor_FiveNineExecutor {
  @available(SwiftStdlib 5.9, *)
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  @available(SwiftStdlib 5.9, *)
  distributed func test(x: Int) throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}
