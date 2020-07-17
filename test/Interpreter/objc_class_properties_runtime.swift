// RUN: %empty-directory(%t)

// RUN: %clang -arch %target-cpu -mmacosx-version-min=10.11 -isysroot %sdk -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o

// RUN: %swiftc_driver -target $(echo '%target-triple' | sed -E -e 's/macosx10.(9|10).*/macosx10.11/') -sdk %sdk -I %S/Inputs/ObjCClasses/ %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import ObjCClasses

class SwiftClass : ProtoWithClassProperty {
  static var getCount = 0
  static var setCount = 0

  private static var _value: CInt = 0

  @objc class func reset() {
    getCount = 0
    setCount = 0
    _value = 0
  }

  @objc class var value: CInt {
    get {
      getCount += 1
      return _value
    }
    set {
      setCount += 1
      _value = newValue
    }
  }

  @objc class var optionalClassProp: Bool {
    return true
  }
}

class Subclass : ClassWithClassProperty {
  static var getCount = 0
  static var setCount = 0
  
  override class func reset() {
    getCount = 0
    setCount = 0
    super.reset()
  }

  override class var value: CInt {
    get {
      getCount += 1
      return super.value
    }
    set {
      setCount += 1
      super.value = newValue
    }
  }

  override class var optionalClassProp: Bool {
    return true
  }
}

var ClassProperties = TestSuite("ClassProperties")

ClassProperties.test("runtime")
  .skip(.osxMinorRange(10, 0...10, reason: "not supported on 10.10 or below"))
  .code {
  let theClass: AnyObject = SwiftClass.self
  let prop = class_getProperty(object_getClass(theClass), "value")
  expectNotNil(prop)

  let nameAsCString = property_getName(prop!)
  expectNotNil(nameAsCString)
  expectEqual("value", String(cString: nameAsCString))
}

runAllTests()

