// RUN: %target-swiftc_driver %s -target %sanitizers-target-triple -g -sanitize=thread -o %t_tsan-binary
// RUN: %target-codesign %t_tsan-binary
// RUN: %target-run %t_tsan-binary 2>&1 | %FileCheck %s --implicit-check-not='ThreadSanitizer'
// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// UNSUPPORTED: OS=tvos

// FIXME: This should be covered by "tsan_runtime"; older versions of Apple OSs
// don't support TSan.
// UNSUPPORTED: remote_run

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

func runOnThread(closure: @escaping () -> Void) {
  let t = T(closure: closure)
  t.start()
}

runOnThread(closure: {
  var oneEmptyArray: [[String:String]] = []
  oneEmptyArray.append(contentsOf: [])
})
runOnThread(closure: {
  var aCompletelyUnrelatedOtherEmptyArray: [[Double:Double]] = []
  aCompletelyUnrelatedOtherEmptyArray.append(contentsOf: [])
})
runOnThread(closure: {
  var array = Array<Int>()
  array.append(contentsOf: [])
})
runOnThread(closure: {
  var arraySlice = ArraySlice<Int>()
  arraySlice.append(contentsOf: [])
})
runOnThread(closure: {
  var contiguousArray = ContiguousArray<Int>()
  contiguousArray.append(contentsOf: [])
})

for _ in 1...5 {
  sem.wait()
}

print("Done!")

// CHECK: Done!
