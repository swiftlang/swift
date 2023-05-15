// RUN: %target-swiftc_driver %s -g -sanitize=thread -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: %target-run %t_tsan-binary 2>&1 | %FileCheck %s --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: foundation
// UNSUPPORTED: OS=tvos

// rdar://101876380
// UNSUPPORTED: OS=ios

import Foundation

let sem = DispatchSemaphore(value: 0)

class T: Thread {
  let closure: () -> Void
  init(closure: @escaping () -> Void) {
    self.closure = closure
  }
  override func main() {
    closure()
    sem.signal()
  }
}

func runOnThread(_ closure: @escaping () -> Void) {
  let t = T(closure: closure)
  t.start()
}

runOnThread {
  var oneEmptyArray: [[String:String]] = []
  oneEmptyArray.append(contentsOf: [])
}
runOnThread {
  var aCompletelyUnrelatedOtherEmptyArray: [[Double:Double]] = []
  aCompletelyUnrelatedOtherEmptyArray.append(contentsOf: [])
}
runOnThread {
  var array = Array<Int>()
  array.append(contentsOf: [])
}
runOnThread {
  var arraySlice = ArraySlice<Int>()
  arraySlice.append(contentsOf: [])
}
runOnThread {
  var contiguousArray = ContiguousArray<Int>()
  contiguousArray.append(contentsOf: [])
}

for _ in 1...5 {
  sem.wait()
}

print("Done!")

// CHECK: Done!
