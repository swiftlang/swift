// RUN: %target-swiftc_driver %s -target %sanitizers-target-triple -g -sanitize=thread -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: %target-run %t_tsan-binary 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: foundation
// UNSUPPORTED: OS=tvos

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

import Foundation

let sem = DispatchSemaphore(value: 0)

class T1: Thread {
  override func main() {
    var oneEmptyArray: [[String:String]] = []
    oneEmptyArray.append(contentsOf: [])
    sem.signal()
  }
}
let t1 = T1()
t1.start()

class T2: Thread {
  override func main() {
    var aCompletelyUnrelatedOtherEmptyArray: [[Double:Double]] = []
    aCompletelyUnrelatedOtherEmptyArray.append(contentsOf: [])
    sem.signal()
  }
}
let t2 = T2()
t2.start()

sem.wait()
sem.wait()
print("Done!")

// CHECK: Done!
