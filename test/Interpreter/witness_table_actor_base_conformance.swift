// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -target %target-swift-5.1-abi-triple -parse-as-library -module-name main -Onone -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

protocol InstancedActor: Actor {
  static var instances: [Layer<Self>] { get }
}

struct Layer<T: Actor> { var value: T }

extension InstancedActor {
  static func firstInstance() -> Self { instances.first!.value }
}

extension MainActor: InstancedActor {
  static var instances: [Layer<MainActor>] {
    [Layer(value: .shared)]
  }
}

@main
struct Main {
  static func main() {
    // Verify that we can successfully create Layer<MainActor>.
    let _ = MainActor.firstInstance()
  }
}
