// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: reflection

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

struct Boom: Error {
  let id: String

  init(file: String = #fileID, line: UInt = #line) {
    self.id = "\(file):\(line)"
  }
  init(id: String) {
    self.id = id
  }
}

struct IgnoredBoom: Error {}
func echo(_ i: Int) async -> Int { i }

func test_taskGroup_throws_rethrows() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_taskGroup_throws_rethrows
  do {
    let got = try await withThrowingTaskGroup(of: Int.self, returning: Int.self) { group in
      group.addTask { await echo(1) }
      group.addTask { await echo(2) }
      group.addTask { throw Boom() }

      do {
        while let r = try await group.next() {
          print("next: \(r)")
        }
      } catch {
        // CHECK: error caught and rethrown in group: Boom(
        print("error caught and rethrown in group: \(error)")
        throw error
      }

      print("should have thrown")
      return 0
    }

    print("Expected error to be thrown, but got: \(got)")
  } catch {
    // CHECK: rethrown: Boom(
    print("rethrown: \(error)")
  }
}

func test_taskGroup_noThrow_ifNotAwaitedThrowingTask() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_taskGroup_noThrow_ifNotAwaitedThrowingTask
  let got = await withThrowingTaskGroup(of: Int.self, returning: Int.self) { group in
    group.addTask { await echo(1) }
    guard let r = try! await group.next() else {
      return 0
    }

    group.addTask { throw Boom() }
    // don't consume this task, so we're not throwing here

    return r
  }

  print("Expected no error to be thrown, got: \(got)") // CHECK: Expected no error to be thrown, got: 1
}

func test_discardingTaskGroup_automaticallyRethrows() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_discardingTaskGroup_automaticallyRethrows
  do {
    let got = try await withThrowingDiscardingTaskGroup(returning: Int.self) { group in
      group.addTask { await echo(1) }
      group.addTask { throw Boom() }
      // add a throwing task, but don't consume it explicitly
      // since we're in discard results mode, all will be awaited and the first error it thrown
      return 13
    }

    print("Expected error to be thrown, but got: \(got)")
  } catch {
    // CHECK: rethrown: Boom(
    print("rethrown: \(error)")
  }
}

func test_discardingTaskGroup_automaticallyRethrowsOnlyFirst() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_discardingTaskGroup_automaticallyRethrowsOnlyFirst
  do {
    let got = try await withThrowingDiscardingTaskGroup(returning: Int.self) { group in
      group.addTask { await echo(1) }
      group.addTask {
        let error = Boom(id: "first, isCancelled:\(Task.isCancelled)")
        print("Throwing: \(error)")
        throw error
      }
      group.addTask {
        // we wait "forever" but since the group will get cancelled after
        // the first error, this will be woken up and throw a cancellation
        do {
          try await Task.sleep(until: .now + .seconds(120), clock: .continuous)
        } catch {
          print("Throwing: \(error)")
          throw error
        }
      }
      return 4
    }

    print("Expected error to be thrown, but got: \(got)")
  } catch {
    // CHECK: Throwing: Boom(id: "first, isCancelled:false
    // CHECK: Throwing: CancellationError()
    // and only then the re-throw happens:
    // CHECK: rethrown: Boom(id: "first
    print("rethrown: \(error)")
  }
}

func test_discardingTaskGroup_automaticallyRethrowsOnlyFirstIncludingGroupBody() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_discardingTaskGroup_automaticallyRethrowsOnlyFirstIncludingGroupBody
  do {
    try await withThrowingDiscardingTaskGroup(returning: Int.self) { group in
      group.addTask { await echo(1) }
      group.addTask {
        try await Task.sleep(until: .now + .seconds(1), clock: .continuous)
        let error = Boom(id: "first, isCancelled:\(Task.isCancelled)")
        print("Throwing: \(error)")
        throw error
      }
      group.addTask {
        // we wait "forever" but since the group will get cancelled after
        // the first error, this will be woken up and throw a cancellation
        try await Task.sleep(until: .now + .seconds(20), clock: .continuous)
      }

      let bodyError = Boom(id: "body, isCancelled:\(group.isCancelled)")
      print("Throwing: \(bodyError)")
      throw bodyError
    }

    print("Expected error to be thrown")
  } catch {
    // CHECK: Throwing: Boom(id: "body, isCancelled:false
    // CHECK: Throwing: Boom(id: "first, isCancelled:true
    // CHECK: Throwing: Boom(id: "second, isCancelled:true
    // and only then the re-throw happens:
    // CHECK: rethrown: Boom(id: "body
    print("rethrown: \(error)")
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_throws_rethrows()
    await test_taskGroup_noThrow_ifNotAwaitedThrowingTask()
    await test_discardingTaskGroup_automaticallyRethrows()
    await test_discardingTaskGroup_automaticallyRethrowsOnlyFirst()
  }
}
