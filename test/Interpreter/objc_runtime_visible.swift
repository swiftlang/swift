// RUN: %empty-directory(%t)

// RUN: %target-clang %target-cc-options -isysroot %sdk -fobjc-arc %S/Inputs/objc_runtime_visible.m -fmodules -nodefaultlibs -lc -dynamiclib -o %t/libobjc_runtime_visible.dylib -install_name @executable_path/libobjc_runtime_visible.dylib
// RUN: %target-codesign %t/libobjc_runtime_visible.dylib
// RUN: nm -g %t/libobjc_runtime_visible.dylib | %FileCheck %s
// RUN: %target-build-swift -import-objc-header %S/Inputs/objc_runtime_visible.h %t/libobjc_runtime_visible.dylib %s -o %t/main
// RUN: %target-run %t/main %t/libobjc_runtime_visible.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop

// CHECK-NOT: HiddenClass

import Foundation
import StdlibUnittest

extension HiddenClass {
  class func create() -> HiddenClass {
    return createHidden()
  }

  func normalMethod() -> String {
    return self.name
  }
}

var ObjCRuntimeVisibleTestSuite = TestSuite("ObjCRuntimeVisible")

ObjCRuntimeVisibleTestSuite.test("methods") {
  let obj = HiddenClass.create()
  expectEqual("Beatrice", obj.name)
  expectEqual("Beatrice", obj.normalMethod())
}

protocol SwiftProto {
  func doTheThing() -> AnyObject
}
extension HiddenClass: SwiftProto {
  func doTheThing() -> AnyObject { return self }
}

func callTheThing<T: SwiftProto>(_ instance: T) -> AnyObject {
  return instance.doTheThing()
}

ObjCRuntimeVisibleTestSuite.test("downcast") {
  let obj = HiddenClass.create()
  let opaque: AnyObject = obj
  let downcasted = opaque as? HiddenClass
  expectNotNil(downcasted)
  expectTrue(obj === downcasted)
}

ObjCRuntimeVisibleTestSuite.test("protocols") {
  let obj = HiddenClass.create()
  expectTrue(obj === obj.doTheThing())

  let protoObj: SwiftProto = obj
  expectTrue(obj === protoObj.doTheThing())

  expectTrue(obj === callTheThing(obj))
}

ObjCRuntimeVisibleTestSuite.test("protocols/downcast")
    .code {
  let obj = HiddenClass.create()
  let opaque: AnyObject = obj
  let downcasted = opaque as? SwiftProto
  expectNotNil(downcasted)
  expectTrue(obj === downcasted!.doTheThing())
}

runAllTests()
