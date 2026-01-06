// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple %s -g -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// FIXME: both of these should work, need to figure out why
// UNSUPPORTED: CPU=arm64e

// Currently this test just checks if nothing crashes.
// TODO: also check if the current executor is the correct one.

final actor MyActor {
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

// FIXME: this breaks if we release the actor during the async main
// because we don't switch off it before dropping the actor reference.
let a = MyActor(p: 27)
@main struct Main {
  static func main() async {
    print("run")
    await print(a.testit())
  }
}


