// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-embedded-synchronization -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -D SYNC_MAIN %s -c -o %t/sync.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/sync.o -o %t/sync.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-embedded-synchronization -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/sync.out | %FileCheck %s --check-prefix=SYNC

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

@_spi(ExperimentalCustomExecutors) import _Concurrency
import Synchronization

final class FactoryMainExecutor: MainExecutor {
  let jobs = Mutex<[UnownedJob]>([])

  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    jobs.withLock { $0.append(job) }
  }

  func run() throws {
    while let job = jobs.withLock({ $0.isEmpty ? nil : $0.removeFirst() }) {
      job.runSynchronously(on: asUnownedSerialExecutor())
    }
  }

  func stop() {}
}

final class FactoryTaskExecutor: TaskExecutor {
  func enqueue(_ job: consuming ExecutorJob) {
    MainActor.executor.enqueue(job)
  }
}

struct Factory: ExecutorFactory {
  static let mainExecutor: any MainExecutor = FactoryMainExecutor()
  static let defaultExecutor: any TaskExecutor = FactoryTaskExecutor()
}

typealias DefaultExecutorFactory = Factory

func assumeMainActor(_ message: StaticString) {
  MainActor.assumeIsolated { print(message) }
}

@main
struct Main {
  #if SYNC_MAIN
  static func main() {
    assumeMainActor("sync main: assumed")
    // SYNC: sync main: assumed
  }
  #else
  static func main() async {
    assumeMainActor("main: assumed")
    // CHECK: main: assumed
    await Task.detached { assumeMainActor("detached task: assumed") }.value
    // CHECK-NEXT: detached task: assumed
    await MainActor.run { assumeMainActor("MainActor.run: assumed") }
    // CHECK-NEXT: MainActor.run: assumed
  }
  #endif
}
