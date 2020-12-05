// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: CPU=arm64e

// Currently this test just checks if nothing crashes.
// TODO: also check if the current executor is the correct one.

final actor class MyActor {
  var p: Int

  @inline(never)
  init(p: Int) {
    self.p = p
  }

  @inline(never)
  func callee() async -> Int {
    print("callee")
    return p
  }

  @inline(never)
  func testit() async -> Int {
    print("don't switch")
    let x = await callee()
    let otherActor = MyActor(p: 12)
    print("switch")
    let y = await otherActor.callee()
    print("switch back")
    return x + y + p
  }
}

// CHECK: run
// CHECK: don't switch
// CHECK: callee
// CHECK: switch
// CHECK: callee
// CHECK: switch back
// CHECK: 66

runAsync {
  let a = MyActor(p: 27)
  print("run")
  await print(a.testit())
}


