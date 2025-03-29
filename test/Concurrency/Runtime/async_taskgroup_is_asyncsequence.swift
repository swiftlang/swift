// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
func test_taskGroup_is_asyncSequence() async {
  print(#function)

  let sum = await withTaskGroup(of: Int.self, returning: Int.self) { group in
    for n in 1...10 {
      group.spawn {
        print("add \(n)")
        return n
      }
    }

    var sum = 0
    for await r in group { // here
      print("next: \(r)")
      sum += r
    }

    return sum
  }

  print("result: \(sum)")
}

@available(SwiftStdlib 5.1, *)
func test_throwingTaskGroup_is_asyncSequence() async throws {
  print(#function)

  let sum = try await withThrowingTaskGroup(of: Int.self, returning: Int.self) { group in
    for n in 1...10 {
      group.spawn {
        print("add \(n)")
        return n
      }
    }

    var sum = 0
    for try await r in group { // here
      print("next: \(r)")
      sum += r
    }

    return sum
  }

  print("result: \(sum)")
}

typealias LabelledTuple = (x: Int, y: Int)

@available(SwiftStdlib 5.1, *)
func test_asyncSequence_labelledTuples() async {
  print(#function)

  let sum = await withTaskGroup(of: LabelledTuple.self, returning: Int.self) { group in
    for n in 1...10 {
      group.spawn {
        print("add (x: \(n), y: 1)")
        return (x: n, y: 1)
      }
    }

    var sum = 0
    for await (x, y) in group { // here
      print("next: (x:\(x), y:\(y))")
      sum += x + y
    }

    return sum
  }

  print("result: \(sum)")
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_taskGroup_is_asyncSequence()
    // CHECK: test_taskGroup_is_asyncSequence()
    // CHECK: result: 55

    try! await test_throwingTaskGroup_is_asyncSequence()
    // CHECK: test_throwingTaskGroup_is_asyncSequence()
    // CHECK: result: 55

    await test_asyncSequence_labelledTuples()
    // CHECK: test_asyncSequence_labelledTuples()
    // CHECK: result: 65
  }
}
