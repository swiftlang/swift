// RUN: %target-run-simple-swift foo | FileCheck %s

// The Objective-C runtime does not implement the autorelease
// optimization on i386, even in the iOS simulator.
// XFAIL: CPU=i386

// XFAIL: interpret

import Foundation

class PrintOnDeinit: NSObject {
  // Result should get autoreleased.
  dynamic class func create() -> PrintOnDeinit {
    return PrintOnDeinit()
  }

  deinit { println("object died") }
}
func useTemp() {
  let f = PrintOnDeinit.create()
}

// objc_retainAutoreleasedReturnValue will fail to reclaim the first
// object autoreleased from each shared object on x86_64, so prime it.
autoreleasepool { useTemp() }
autoreleasepool {
  println("autorelease test begin")
  useTemp()
  println("after call to useTemp")
}
println("autorelease test end")
// CHECK:      autorelease test begin
// CHECK-NEXT: object died
// CHECK-NEXT: after call to useTemp
// CHECK-NEXT: autorelease test end
