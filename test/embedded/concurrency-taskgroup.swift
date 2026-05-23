// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lc++abi -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

protocol Go: Actor {
  var name: String { get }
  func go(times: Int) async -> Int
}

extension Go {
  func go(times: Int) async -> Int {
    for i in 0..<times {
      print("\(name) @ \(i)")
      await Task.yield()
    }
    return times
  }
}

actor One: Go { var name = "One" }
actor Two: Go { var name = "Two" }

func yielding() async {
  let one = One()
  let two = Two()
  await withTaskGroup(of: Int.self) { group in
    group.addTask {
      await one.go(times: 5)
    }
    group.addTask {
      await two.go(times: 5)
    }
  }
}

enum HomeworkError: Error {
case dogAteIt
}

func throwing() async throws {
  print("Ready to throw")
  let one = One()
  let two = Two()
  try await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask {
      await one.go(times: 5)
    }
    group.addTask {
      throw HomeworkError.dogAteIt
    }

    _ = try await group.next()
    _ = try await group.next()
  }
}

@main struct Main {
  static func main() async {
    await yielding()
    print("All done!")
    // CHECK: One @ 0
    // CHECK: Two @ 0
    // CHECK: One @ 1
    // CHECK: Two @ 1
    // CHECK: One @ 2
    // CHECK: Two @ 2
    // CHECK: One @ 3
    // CHECK: Two @ 3
    // CHECK: One @ 4
    // CHECK: Two @ 4
    // CHECK: All done!

    // CHECK: Ready to throw
    do {
      try await throwing()
    } catch let error as HomeworkError {
      // CHECK: Caught HomeworkError
      print("Caught HomeworkError")
    } catch {
      fatalError("Couldn't match HomeworkError")
    }
  }
}
