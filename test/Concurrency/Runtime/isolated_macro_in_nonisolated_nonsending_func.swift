// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// rdar://78109470
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency
import StdlibUnittest

@main struct Main {
  static func main() async {
    await explicitIsolatedParam()
    // CHECK: go - explicitIsolatedParam
    // CHECK: outerIsolation = #isolation = Optional(Swift.MainActor)
    // CHECK: Task{} #isolation = nil
    // CHECK: Task{ [outerIsolation] } outerIsolation = Optional(Swift.MainActor), #iso = Optional(Swift.MainActor)
    // CHECK: done - explicitIsolatedParam

    print()
    await nonisolatedNonsending()
    // CHECK: go - nonisolatedNonsending
    // CHECK: outerIsolation = #isolation = Optional(Swift.MainActor)
    // CHECK: Task{} #isolation = nil
    // CHECK: Task{ [outerIsolation] } outerIsolation = Optional(Swift.MainActor), #iso = nil
    // CHECK: done - nonisolatedNonsending
  }
}

func explicitIsolatedParam(isolation: isolated (any Actor)? = #isolation) async {
  print("go - \(#function)")
  MainActor.assertIsolated()

  let outerIsolation = #isolation
  print("outerIsolation = #isolation = \(String(describing: outerIsolation))")

  await Task {
    let iso = #isolation
    print("Task{} #isolation = \(String(describing: iso))")
  }.value

  await Task {
    let iso = #isolation
    print("Task{ [outerIsolation] } outerIsolation = \(String(describing: isolation)), #iso = \(String(describing: iso))")
  }.value

  print("done - \(#function)")
}


nonisolated(nonsending) func nonisolatedNonsending() async {
  print("go - \(#function)")
  MainActor.assertIsolated()

  let outerIsolation = #isolation
  print("outerIsolation = #isolation = \(String(describing: outerIsolation))") // WRONG; this is nil today

  await Task {
    let iso = #isolation
    print("Task{} #isolation = \(String(describing: iso))")
  }.value

  await Task {
    let iso = #isolation
    print("Task{ [outerIsolation] } outerIsolation = \(String(describing: outerIsolation)), #iso = \(String(describing: iso))")
  }.value

  print("done - \(#function)")
}
