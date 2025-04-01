// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -target %target-cpu-apple-macos14 -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
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
    }
}
