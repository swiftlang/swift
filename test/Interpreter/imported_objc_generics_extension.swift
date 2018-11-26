// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// XFAIL: interpret
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import ObjCClasses

var ImportedObjCGenericExtension = TestSuite("ImportedObjCGenericExtension")

@objc extension Container {
  @objc func returnSelf() -> Self {
    return self
  }
}

ImportedObjCGenericExtension.test("ExtensionFromSwift") {
  let gc = Container<NSString>(object: "")
  expectTrue(gc.returnSelf() === gc)
  let gc2: Unmanaged<AnyObject>! = gc.perform(#selector(Container<NSString>.returnSelf))
  expectTrue(gc2!.takeUnretainedValue() === gc)
}

runAllTests()
