// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

actor Simple {
  var count = 0
  func report() {
    print("simple.count == \(count)")
    count += 1
  }
}

actor Custom {
  var count = 0
  let simple = Simple()

  @available(SwiftStdlib 5.5, *)
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("custom unownedExecutor")
    return simple.unownedExecutor
  }

  func report() async {
    print("custom.count == \(count)")
    count += 1

    await simple.report()
  }
}

@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() async {
    print("begin")
    let actor = Custom()
    await actor.report()
    await actor.report()
    await actor.report()
    print("end")
  }
}

// CHECK:      begin
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 0
// CHECK-NEXT: simple.count == 0
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 1
// CHECK-NEXT: simple.count == 1
// CHECK-NEXT: custom unownedExecutor
// CHECK-NEXT: custom.count == 2
// CHECK-NEXT: simple.count == 2
// CHECK-NEXT: end
