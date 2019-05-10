// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

@objc(MyObjCProtocol) public protocol MyProtocol { }

public protocol OtherProtocol {
  associatedtype AssocType
}

public struct MyThing: OtherProtocol {
  public typealias AssocType = MyProtocol
}

func getAssocType<T: OtherProtocol>(_: T.Type) -> Any.Type {
  return T.AssocType.self
}
let RenamedObjCDemangleTests = TestSuite("RenamedObjCDemangle")

RenamedObjCDemangleTests.test("@objc protocols") {
  expectEqual(getAssocType(MyThing.self), MyProtocol.self)
}

@objc(MyObjCClass) class MyClass: NSObject { }

struct MyClassWrapper: OtherProtocol {
  typealias AssocType = MyClass
}

RenamedObjCDemangleTests.test("@objc classes") {
  expectEqual(getAssocType(MyClassWrapper.self), MyClass.self)
}

runAllTests()
