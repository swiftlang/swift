// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/../Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -import-bridging-header %S/../../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/main.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -import-bridging-header %S/../../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/assume.o -D WORKER_ASSUMES_MAIN_ACTOR
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -import-bridging-header %S/../../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/noworkers.o -D NO_WORKERS
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/pool.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_ConcurrencyThreadPoolExecutor -lswift_Concurrency %target-embedded-concurrency-threading-shim %target-embedded-synchronization -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/after.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency -lswift_ConcurrencyThreadPoolExecutor %target-embedded-concurrency-threading-shim %target-embedded-synchronization -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/cooperative.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_Concurrency %target-embedded-concurrency-threading-shim %target-embedded-synchronization -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/assume.o %t/unbuffered-putchar.o -o %t/assume.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_ConcurrencyThreadPoolExecutor -lswift_Concurrency %target-embedded-concurrency-threading-shim %target-embedded-synchronization -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/noworkers.o -o %t/noworkers.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lswift_ConcurrencyThreadPoolExecutor -lswift_Concurrency %target-embedded-concurrency-threading-shim %target-embedded-synchronization -dead_strip
// RUN: env %env-SWIFT_HARDWARE_CONCURRENCY=1 %target-run %t/pool.out | %FileCheck %s --check-prefixes=CHECK,POOL,ONE
// RUN: env %env-SWIFT_HARDWARE_CONCURRENCY=2 %target-run %t/pool.out | %FileCheck %s --check-prefixes=CHECK,POOL,TWO
// RUN: env %env-SWIFT_HARDWARE_CONCURRENCY=1 %target-run %t/after.out | %FileCheck %s --check-prefixes=CHECK,COOPERATIVE,ONE
// RUN: env %env-SWIFT_HARDWARE_CONCURRENCY=1 %target-run %t/cooperative.out | %FileCheck %s --check-prefixes=CHECK,COOPERATIVE,ONE
// RUN: %target-not-crash %target-run %t/assume.out 2>&1 | %FileCheck %s --check-prefix=ASSUME
// RUN: %target-run %t/noworkers.out | %FileCheck %s --check-prefix=NO-WORKERS
// RUN: printenv %env-SWIFT_HARDWARE_CONCURRENCY | %FileCheck %s --check-prefix=LIT

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: wasi_threads
// REQUIRES: swift_feature_Embedded

// LIT: {{^}}{{[1-9][0-9]*}}{{$}}

import Synchronization
import WASILibc

actor Recorder {
  var ranOnMainThread = true
  func record() { ranOnMainThread = _swift_thread_isMain() != 0 }
}

func onMainThread() -> Bool { _swift_thread_isMain() != 0 }

func assumeMainActor() {
  MainActor.assumeIsolated { print("assumed the main actor") }
}

func allocateSemaphore() -> UnsafeMutableRawPointer {
  let semaphore = UnsafeMutableRawPointer.allocate(
    byteCount: Int(EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS)
      * MemoryLayout<UnsafeRawPointer>.size,
    alignment: MemoryLayout<UnsafeRawPointer>.alignment)
  _swift_sem_init(semaphore, 0)
  return semaphore
}

let arrivals = Atomic<Int>(0)

// Both callers return true only when the two run at the same time.
func meet() -> Bool {
  arrivals.wrappingAdd(1, ordering: .sequentiallyConsistent)
  let deadline = time(nil) + 60
  while arrivals.load(ordering: .sequentiallyConsistent) < 2 {
    if time(nil) >= deadline {
      return false
    }
  }
  return true
}

let globalRan = Atomic<Bool>(false)
let lowRan = Atomic<Bool>(false)

@MainActor var mainActorCounter = 0

@main
struct Main {
  static func main() async {
#if WORKER_ASSUMES_MAIN_ACTOR
    print("assuming the main actor on a worker")
    // ASSUME: assuming the main actor on a worker
    await Task.detached { assumeMainActor() }.value
    // ASSUME-NOT: assumed the main actor
#elseif NO_WORKERS
    // Exhaust the host's thread limit before the first task, so that the pool
    // cannot start a worker.
    let gate = allocateSemaphore()
    let capacity = 256
    var created = 0
    while created < capacity {
      let started = _swift_thread_create({ gate in
        _swift_sem_wait(gate!)
        return nil
      }, gate)
      if started == 0 {
        break
      }
      created += 1
    }
    print("refused a thread: \(created < capacity)")
    // NO-WORKERS: refused a thread: true
    let detached = await Task.detached { onMainThread() }.value
    print("detached task on the main thread: \(detached)")
    // NO-WORKERS: detached task on the main thread: true

    // The main thread runs global and main-actor jobs, highest priority first.
    let low = Task.detached(priority: .low) { () -> Bool in
      let high = Task.detached(priority: .high) {
        globalRan.store(true, ordering: .sequentiallyConsistent)
      }
      let ranFirst = await MainActor.run {
        globalRan.load(ordering: .sequentiallyConsistent)
      }
      await high.value
      return ranFirst
    }
    print("high-priority job ran before a main-actor hop: \(await low.value)")
    // NO-WORKERS: high-priority job ran before a main-actor hop: true
    let high = Task.detached(priority: .high) { () -> Bool in
      let low = Task.detached(priority: .low) {
        lowRan.store(true, ordering: .sequentiallyConsistent)
      }
      let ranFirst = await MainActor.run {
        !lowRan.load(ordering: .sequentiallyConsistent)
      }
      await low.value
      return ranFirst
    }
    print("main-actor hop ran before a low-priority job: \(await high.value)")
    // NO-WORKERS: main-actor hop ran before a low-priority job: true

    for _ in 0..<created {
      _swift_sem_post(gate)
    }
#else
    print("main on the main thread: \(onMainThread())")
    // CHECK: main on the main thread: true
    assumeMainActor()
    // CHECK: assumed the main actor

    let recorder = Recorder()
    await recorder.record()
    print("actor on the main thread: \(await recorder.ranOnMainThread)")
    // POOL: actor on the main thread: false
    // COOPERATIVE: actor on the main thread: true

    let detached = await Task.detached { onMainThread() }.value
    print("detached task on the main thread: \(detached)")
    // POOL: detached task on the main thread: false
    // COOPERATIVE: detached task on the main thread: true

    let viaMainActor = await Task.detached {
      await MainActor.run { onMainThread() }
    }.value
    print("MainActor.run on the main thread: \(viaMainActor)")
    // CHECK: MainActor.run on the main thread: true

    await withTaskGroup(of: Void.self) { group in
      for _ in 0..<100 {
        group.addTask { @MainActor in mainActorCounter += 1 }
      }
    }
    print("main actor counter: \(mainActorCounter)")
    // CHECK: main actor counter: 100

    if _swift_thread_getHardwareConcurrency() >= 2 {
      async let a = meet()
      async let b = meet()
      let (metA, metB) = await (a, b)
      print("children met: \(metA && metB)")
    }
    // TWO: children met: true
    // ONE-NOT: children met

    print("main on the main thread after awaits: \(onMainThread())")
    // CHECK: main on the main thread after awaits: true
#endif
  }
}
