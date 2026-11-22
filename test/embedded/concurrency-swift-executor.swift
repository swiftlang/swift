// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/optout.o -D OPTOUT -D NONISOLATED
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/allmain.o -D ALL_MAIN -D NONISOLATED
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/mainref.o -D MAIN_REF
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/genericref.o -D GENERIC_REF
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/genericref.o %t/unbuffered-putchar.o -o %t/genericref.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/optout.o -o %t/optout.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/allmain.o -o %t/allmain.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/mainref.o -o %t/mainref.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-cxx-lib -lswift_Concurrency %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-not-crash %target-run %t/genericref.out 2>&1 | %FileCheck %s --check-prefix=GEN
// RUN: %target-run %t/optout.out | %FileCheck %s --check-prefixes=CHECK,NI
// RUN: %target-run %t/allmain.out | %FileCheck %s --check-prefixes=CHECK,NI
// RUN: %target-run %t/mainref.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_embedded_platform

// An executor written in Swift implements the Concurrency PAL's executor
// functions with @c and links no executor archive. Without the opt-out,
// main-queue jobs must run with swift_executor_main(), or
// MainActor.assumeIsolated traps.

import _Concurrency
import SwiftPlatform

@MainActor var counter = 0

@MainActor func bump() -> Int {
  counter += 1
  return counter
}

func assumeMainActor(_ message: StaticString) {
  MainActor.assumeIsolated { print(message) }
}

@main
struct Main {
  static func main() async {
    print("main: \(bump())")
    // CHECK: main: 1
    // GEN: main: 1
    assumeMainActor("main: assumed")
    // CHECK-NEXT: main: assumed
    // GEN-NEXT: main: assumed
    let fromDetached = await Task.detached { await bump() }.value
    print("detached: \(fromDetached)")
    // CHECK-NEXT: detached: 2
    // GEN-NEXT: detached: 2
    // GEN-NOT: main-actor task: assumed
    await Task { @MainActor in assumeMainActor("main-actor task: assumed") }.value
    // CHECK-NEXT: main-actor task: assumed
    await MainActor.run { assumeMainActor("MainActor.run: assumed") }
    // CHECK-NEXT: MainActor.run: assumed
#if NONISOLATED
    await Task.detached { assumeMainActor("detached task: assumed") }.value
    // NI-NEXT: detached task: assumed
#endif
    print("done")
    // CHECK-NEXT: done
  }
}

// The executor: one FIFO queue threaded through the jobs' scheduler-private
// words. Word 0 links to the next job, and word 1 is non-nil for a job that
// was enqueued on the main executor.

nonisolated(unsafe) var head: UnsafeMutablePointer<SwiftJob>? = nil
nonisolated(unsafe) var tail: UnsafeMutablePointer<SwiftJob>? = nil

func push(_ job: UnsafeMutablePointer<SwiftJob>, isMain: Bool) {
  let data = swift_job_getPrivateData(job)
  data[0] = nil
  data[1] = isMain ? UnsafeMutableRawPointer(bitPattern: 1) : nil
  if let last = tail {
    swift_job_getPrivateData(last)[0] = UnsafeMutableRawPointer(job)
  } else {
    head = job
  }
  tail = job
}

func pop() -> (job: UnsafeMutablePointer<SwiftJob>, isMain: Bool)? {
  guard let job = head else { return nil }
  let data = swift_job_getPrivateData(job)
  let isMain = data[1] != nil
  if let next = data[0] {
    head = next.assumingMemoryBound(to: SwiftJob.self)
  } else {
    head = nil
    tail = nil
  }
  return (job, isMain)
}

@c(_swift_concurrency_enqueueGlobal)
func enqueueGlobal(_ job: UnsafeMutablePointer<SwiftJob>) {
  push(job, isMain: false)
}

@c(_swift_concurrency_enqueueMainExecutor)
func enqueueMainExecutor(_ job: UnsafeMutablePointer<SwiftJob>) {
  push(job, isMain: true)
}

#if OPTOUT
@c(_swift_concurrency_mainExecutorIsGlobal)
func mainExecutorIsGlobal() -> Int {
  1
}
#endif

@c(_swift_concurrency_asyncMain)
func asyncMain() -> Never {
  while true {
    guard let (job, isMain) = pop() else {
      print("executor: queue empty")
      _swift_exit(3)
    }
#if OPTOUT
    _ = isMain
    swift_job_runSynchronously(job, swift_executor_generic())
#elseif ALL_MAIN
    _ = isMain
    swift_job_runSynchronously(job, swift_executor_main())
#elseif GENERIC_REF
    _ = isMain
    swift_job_runSynchronously(job, swift_executor_generic())
#else
    swift_job_runSynchronously(
      job, isMain ? swift_executor_main() : swift_executor_generic())
#endif
  }
}
