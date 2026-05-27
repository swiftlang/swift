// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation_consuming.h) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// A class whose deallocation we can observe.
class Witness : NSObject {
  var label: String

  init(label: String) {
    self.label = label
  }

  deinit {
    print("\(label) deinit")
  }
}

@objc @implementation extension ObjCClass {
  // The ObjC header declares takeObject: as a regular (non-consuming) method.
  // The Swift implementation uses 'consuming' ownership. The ObjC thunk must
  // copy the argument so the Swift function consumes the copy, not the
  // caller's original reference.
  func take(_ obj: consuming NSObject) {
    _ = obj
  }

  class func runTests() {
    let c = ObjCClass()
    let w = Witness(label: "witness")
    // Call through ObjC dispatch. The ObjC caller passes 'w' at +0.
    // If the thunk incorrectly treats it as +1 (consumed), it will
    // over-release 'w', causing a crash on the next access.
    callTakeObjectFromObjC(c, w)
    // CHECK: after takeObject: witness
    print("after takeObject: \(w.label)")
  }
}

// CHECK: witness deinit
ObjCClass.runTests()
