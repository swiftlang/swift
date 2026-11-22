// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

@_spi(ExperimentalCustomExecutors) import _Concurrency

final class FactoryMainExecutor: MainExecutor {
  func enqueue(_ job: consuming ExecutorJob) { fatalError() }
  func run() throws { fatalError() }
  func stop() {}
}

final class FactoryTaskExecutor: TaskExecutor {
  func enqueue(_ job: consuming ExecutorJob) { fatalError() }
}

struct Factory: ExecutorFactory {
  static let mainExecutor: any MainExecutor = FactoryMainExecutor()
  static let defaultExecutor: any TaskExecutor = FactoryTaskExecutor()
}

typealias DefaultExecutorFactory = Factory

final class OtherExecutor: SerialExecutor {
  func enqueue(_ job: consuming ExecutorJob) {}
}

@main
struct Main {
  static func main() async {
    await MainActor.run { print("MainActor.run: ran") }
    // CHECK: MainActor.run: ran
    await Task.detached { print("detached: ran") }.value
    // CHECK-NEXT: detached: ran
    print(MainActor.executor.isMainExecutor ? "MainActor.executor: main" : "MainActor.executor: other")
    // CHECK-NEXT: MainActor.executor: main
    print(OtherExecutor().isMainExecutor ? "other: main" : "other: not main")
    // CHECK-NEXT: other: not main
  }
}
