// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://124277662
// XFAIL: freestanding

actor MyActor: CustomStringConvertible {
  let description = "MyActor"
}

func optionalIsolated(to actor: isolated (any Actor)?) {
  actor?.assertIsolated()
  if let actor {
    print("isolated to \(actor)")
  } else {
    print("nonisolated")
  }
}

// CHECK: nonisolated
// CHECK: isolated to Swift.MainActor
// CHECK: isolated to MyActor
optionalIsolated(to: nil)
optionalIsolated(to: MainActor.shared)
await optionalIsolated(to: MyActor())
