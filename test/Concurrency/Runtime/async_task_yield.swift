// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// https://bugs.swift.org/browse/SR-14333
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
protocol Go: Actor {
  func go(times: Int) async -> Int
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension Go {
  func go(times: Int) async -> Int {
    for i in 0...times {
      print("\(Self.self) @ \(i)")
      await Task.yield()
    }
    return times
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor One: Go {}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor Two: Go {}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func yielding() async {
  let one = One()
  let two = Two()
  await withTaskGroup(of: Int.self) { group in
    await group.spawn {
      await one.go(times: 100)
    }
    await group.spawn {
      await two.go(times: 100)
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@main struct Main {
  static func main() async {
    await yielding()
    // TODO: No idea for a good test for this... Open to ideas?
    // CHECK: Two @ 100
  }
}
