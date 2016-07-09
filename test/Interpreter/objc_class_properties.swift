// RUN: rm -rf %t && mkdir -p %t

// RUN: %clang %target-cc-options -isysroot %sdk -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// XFAIL: interpret
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

  @objc class func value() -> CInt {
    getCount += 1
    return _value
  }
  @objc class func setValue(_ newValue: CInt) {
    setCount += 1
    _value = newValue
  }

  @objc class func optionalClassProp() -> Bool {
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

  override class func value() -> CInt {
    getCount += 1
    return super.value()
  }
  override class func setValue(_ newValue: CInt) {
    setCount += 1
    super.setValue(newValue)
  }

  override class func optionalClassProp() -> Bool {
    return true
  }
}

var ClassProperties = TestSuite("ClassProperties")

ClassProperties.test("direct") {
  ClassWithClassProperty.reset()
  expectEqual(0, ClassWithClassProperty.value())
  ClassWithClassProperty.setValue(4)
  expectEqual(4, ClassWithClassProperty.value())

  Subclass.reset()
  expectEqual(0, Subclass.value())
  Subclass.setValue(4)
  expectEqual(4, Subclass.value())
  expectEqual(2, Subclass.getCount)
  expectEqual(1, Subclass.setCount)
}

func testExistential(e: ProtoWithClassProperty.Type) {
  e.reset()
  expectEqual(0, e.value())
  e.setValue(4)
  expectEqual(4, e.value())
}

ClassProperties.test("existentials") {
  testExistential(ClassWithClassProperty.self)
  testExistential(ObjCSubclassWithClassProperty.self)

  testExistential(SwiftClass.self)
  expectEqual(2, SwiftClass.getCount)
  expectEqual(1, SwiftClass.setCount)

  testExistential(Subclass.self)
  expectEqual(2, Subclass.getCount)
  expectEqual(1, Subclass.setCount)
}

func testGeneric<T: ProtoWithClassProperty>(e: T.Type) {
  e.reset()
  expectEqual(0, e.value())
  e.setValue(4)
  expectEqual(4, e.value())
}

ClassProperties.test("generics") {
  testGeneric(ClassWithClassProperty.self)
  testGeneric(ObjCSubclassWithClassProperty.self)

  testGeneric(SwiftClass.self)
  expectEqual(2, SwiftClass.getCount)
  expectEqual(1, SwiftClass.setCount)

  testGeneric(Subclass.self)
  expectEqual(2, Subclass.getCount)
  expectEqual(1, Subclass.setCount)
}

func testInheritance(e: ClassWithClassProperty.Type) {
  e.reset()
  expectEqual(0, e.value())
  e.setValue(4)
  expectEqual(4, e.value())
}

ClassProperties.test("inheritance") {
  testInheritance(ClassWithClassProperty.self)
  testInheritance(ObjCSubclassWithClassProperty.self)

  testInheritance(Subclass.self)
  expectEqual(2, Subclass.getCount)
  expectEqual(1, Subclass.setCount)
}

func testInheritanceGeneric<T: ClassWithClassProperty>(e: T.Type) {
  e.reset()
  expectEqual(0, e.value())
  e.setValue(4)
  expectEqual(4, e.value())
}

ClassProperties.test("inheritance/generic") {
  testInheritanceGeneric(ClassWithClassProperty.self)
  testInheritanceGeneric(ObjCSubclassWithClassProperty.self)

  testInheritanceGeneric(Subclass.self)
  expectEqual(2, Subclass.getCount)
  expectEqual(1, Subclass.setCount)
}

ClassProperties.test("optionalProp") {
  let noProp: ProtoWithClassProperty.Type = ClassWithClassProperty.self
  expectEmpty(noProp.optionalClassProp)

  let hasProp: ProtoWithClassProperty.Type = Subclass.self
  expectNotEmpty(hasProp.optionalClassProp)
  expectEqual(true, hasProp.optionalClassProp!())

  let hasOwnProp: ProtoWithClassProperty.Type = SwiftClass.self
  expectNotEmpty(hasOwnProp.optionalClassProp)
  expectEqual(true, hasOwnProp.optionalClassProp!())

  let hasPropObjC: ProtoWithClassProperty.Type = ObjCSubclassWithClassProperty.self
  expectNotEmpty(hasPropObjC.optionalClassProp)
  expectEqual(true, hasPropObjC.optionalClassProp!())
}

runAllTests()

