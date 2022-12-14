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

func test_taskGroup_discardResults_automaticallyRethrows() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_taskGroup_discardResults_automaticallyRethrows
  do {
    let got = try await withThrowingTaskGroup(of: Int.self, returning: Int.self,
                                              discardResults: true) { group in
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

func test_taskGroup_discardResults_automaticallyRethrowsOnlyFirst() async {
  print("==== \(#function) ------") // CHECK_LABEL: test_taskGroup_discardResults_automaticallyRethrowsOnlyFirst
  do {
    let got = try await withThrowingTaskGroup(of: Int.self, returning: Int.self,
                                              discardResults: true) { group in
      group.addTask { await echo(1) }
      group.addTask { throw Boom(id: "first") }
      // add a throwing task, but don't consume it explicitly
      // since we're in discard results mode, all will be awaited and the first error it thrown

      do {
        try await group.waitForAll()
      } catch {
        // CHECK: caught: Boom(id: "first")
        print("caught: \(error)")
      }

      group.addTask { throw Boom(id: "second") }

      return 4
    }

    print("Expected error to be thrown, but got: \(got)")
  } catch {
    // CHECK: rethrown: Boom(id: "second")
    print("rethrown: \(error)")
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_throws_rethrows()
    await test_taskGroup_noThrow_ifNotAwaitedThrowingTask()
    await test_taskGroup_discardResults_automaticallyRethrows()
    await test_taskGroup_discardResults_automaticallyRethrowsOnlyFirst()
  }
}
