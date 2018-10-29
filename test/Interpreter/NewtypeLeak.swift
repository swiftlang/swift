// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/newtype.m -o %t/newtype.objc.o -I %S/Inputs/usr/include -fmodules
// RUN: %target-build-swift -c -I %S/Inputs/usr/include -o %t/newtype.swift.o %s
// RUN: %target-swiftc_driver %t/newtype.objc.o %t/newtype.swift.o -o %t/newtype
// RUN: %target-codesign %t/newtype
// RUN: %target-run %t/newtype

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest
import Newtype

class ObjCLifetimeTracked : NSMyObject {
  var a = LifetimeTracked(0)
}

// Make sure that we do properly autorelease newtypes and do not leak them.
class ObjCTest : NSObject {
  @objc dynamic func optionalPassThrough(_ ed: MyObject?) -> MyObject? {
    return ed
  }
}

func main() {
  let e = MyObject(ObjCLifetimeTracked())
  let c = ObjCTest()
  let x = c.optionalPassThrough(e)!
  x.rawValue.print()
}

var Tests = TestSuite("newtypeleak")

Tests.test("dontLeak") {
  autoreleasepool {
    main()
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()
