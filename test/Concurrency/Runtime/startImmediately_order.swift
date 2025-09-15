// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s %import-libdispatch -swift-version 6 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import _Concurrency

let max = 1000

func bar(x: Int, cc: CheckedContinuation<Void, Never>) {
  Task.immediate {
    print("Task \(x) started")
    try! await Task.sleep(nanoseconds: 10000)
    if (x == max) {
      cc.resume()
    }
  }
}

await withCheckedContinuation { (cc: CheckedContinuation<Void, Never>) in
  for i in 1...max {
    bar(x: i, cc: cc)
  }
}

// CHECK: Task 1 started
// CHECK: Task 2 started
// CHECK: Task 3 started
// CHECK: Task 4 started
// CHECK: Task 5 started
// CHECK: Task 6 started
// CHECK: Task 7 started
// CHECK: Task 8 started
// CHECK: Task 9 started
// CHECK: Task 10 started
// CHECK: Task 11 started
// CHECK: Task 12 started
// CHECK: Task 13 started
// CHECK: Task 14 started
// CHECK: Task 15 started
// CHECK: Task 16 started
// CHECK: Task 17 started
// CHECK: Task 18 started
// CHECK: Task 19 started
// CHECK: Task 20 started
// CHECK: Task 21 started
// CHECK: Task 22 started
// CHECK: Task 23 started
// CHECK: Task 24 started
// CHECK: Task 25 started
// CHECK: Task 26 started
// CHECK: Task 27 started
// CHECK: Task 28 started
// CHECK: Task 29 started
// CHECK: Task 30 started
