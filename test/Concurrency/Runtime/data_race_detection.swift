// RUN: %empty-directory(%t)
// RUN: %target-build-swift %import-libdispatch -Xfrontend -disable-availability-checking -enable-actor-data-race-checks -parse-as-library %s -o %t/a.out -module-name main
// RUN: %target-codesign %t/a.out
// RUN: env %env-SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL=1 %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: single_threaded_concurrency

import _Concurrency
import Dispatch

// For sleep
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@MainActor func onMainActor() {
  print("I'm on the main actor!")
}

func promiseMainThread(_ fn: @escaping @MainActor () -> Void) -> (() -> Void) {
  typealias Fn = () -> Void
  return unsafeBitCast(fn, to: Fn.self)
}

func launchTask(_ fn: @escaping () -> Void) {
  if #available(macOS 10.10, iOS 8.0, watchOS 2.0, tvOS 8.0, *) {
    DispatchQueue.global().async {
      fn()
    }
  }
}

func launchFromMainThread() {
  launchTask(promiseMainThread(onMainActor))
}

actor MyActor {
  var counter = 0

  func onMyActor() {
    counter = counter + 1
  }

  func getTaskOnMyActor() -> (() -> Void) {
    return {
      self.onMyActor()
    }
  }
}

@main
struct Runner {
  static func main() async {
    print("Launching a main-actor task")
    // CHECK: warning: data race detected: @MainActor function at main/data_race_detection.swift:25 was not called on the main thread
    launchFromMainThread()
    sleep(1)

    let actor = MyActor()
    let actorFn = await actor.getTaskOnMyActor()
    print("Launching an actor-instance task")
    // CHECK: warning: data race detected: actor-isolated function at main/data_race_detection.swift:54 was not called on the same actor
    launchTask(actorFn)

    sleep(1)
  }
}
