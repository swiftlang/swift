// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

protocol Go: Actor {
  func go(times: Int) async -> Int
}
extension Go {
  func go(times: Int) async -> Int {
    for i in 0...times {
      print("\(Self.self) @ \(i)")
      await Task.yield()
    }
    return times
  }
}

actor One: Go {}
actor Two: Go {}

func yielding() async {
  let one = One()
  let two = Two()
  try! await Task.withGroup(resultType: Int.self) { group in
    await group.add {
      await one.go(times: 100)
    }
    await group.add {
      await two.go(times: 100)
    }
  }
}

@main struct Main {
  static func main() async {
    await yielding()
    // TODO: No idea for a good test for this... Open to ideas?
    // CHECK:      One
    // CHECK-NEXT: Two
  }
}
