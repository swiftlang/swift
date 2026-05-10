// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar81421394.m -I %S/Inputs -c -o %t/rdar81421394.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar81421394.h -Xlinker %t/rdar81421394.o %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Foundation

protocol RangeFinder {
    var range: NSRange { get set }
}

extension Ranger : RangeFinder {}

class Wrapper {
  let instance: Ranger<AnyObject, NSObject>

  init() {
    instance = Ranger()
  }

  var range: NSRange {
    _read {
      yield instance.range
    }
    _modify {
      print("staring modify")
      yield &instance.range
      print("ending modify")
    }
  }
}

let w = Wrapper()
print("begin:", w.range)
// CHECK: begin: {0, 0}
w.range = NSRange(location: 3, length: 5)
// CHECK: end: {3, 5}
print("end:", w.range)
