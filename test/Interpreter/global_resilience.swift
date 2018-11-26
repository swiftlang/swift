// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/libresilient_struct.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct
// RUN: %target-codesign %t/libresilient_struct.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_global.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_global.swift -emit-module -emit-module-path %t/resilient_global.swiftmodule -module-name resilient_global -I%t -L%t -lresilient_struct
// RUN: %target-codesign %t/libresilient_global.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct -lresilient_global -o %t/main -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/libresilient_struct.%target-dylib-extension %t/libresilient_global.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_struct_wmo.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_struct.swift -emit-module -emit-module-path %t/resilient_struct.swiftmodule -module-name resilient_struct -whole-module-optimization
// RUN: %target-codesign %t/libresilient_struct_wmo.%target-dylib-extension

// RUN: %target-build-swift-dylib(%t/libresilient_global_wmo.%target-dylib-extension) -Xfrontend -enable-resilience %S/../Inputs/resilient_global.swift -emit-module -emit-module-path %t/resilient_global.swiftmodule -module-name resilient_global -I%t -L%t -lresilient_struct_wmo -whole-module-optimization
// RUN: %target-codesign %t/libresilient_global_wmo.%target-dylib-extension

// RUN: %target-build-swift %s -L %t -I %t -lresilient_struct_wmo -lresilient_global_wmo -o %t/main2 -Xlinker -rpath -Xlinker %t
// RUN: %target-codesign %t/main2

// RUN: %target-run %t/main2 %t/libresilient_struct_wmo.%target-dylib-extension %t/libresilient_global_wmo.%target-dylib-extension

// REQUIRES: executable_test

import StdlibUnittest


import resilient_global
import resilient_struct

var ResilientGlobalTestSuite = TestSuite("ResilientGlobal")

//
// Fits inside a buffer's inline storage.
//

public struct MySmallResilientStruct {
  let x: Int32
}

let mySmallGlobal = MySmallResilientStruct(x: 1)

ResilientGlobalTestSuite.test("MySmallGlobal") {
  expectEqual(1, mySmallGlobal.x)
}

//
// Requires out-of-line allocation.
//

public struct MyLargeResilientStruct {
  let w: Int64
  let x: Int64
  let y: Int64
  let z: Int64
}

var myLargeGlobal = MyLargeResilientStruct(w: 1, x: 2, y: 3, z: 4)

ResilientGlobalTestSuite.test("MyLargeGlobal") {
  expectEqual(1, myLargeGlobal.w)
  expectEqual(2, myLargeGlobal.x)
  expectEqual(3, myLargeGlobal.y)
  expectEqual(4, myLargeGlobal.z)

  myLargeGlobal = MyLargeResilientStruct(w: 5, x: 6, y: 7, z: 8)
  expectEqual(5, myLargeGlobal.w)
  expectEqual(6, myLargeGlobal.x)
  expectEqual(7, myLargeGlobal.y)
  expectEqual(8, myLargeGlobal.z)
}

let myLargeGlobalUninitialized: MyLargeResilientStruct

myLargeGlobalUninitialized = MyLargeResilientStruct(w: 9, x: 10, y: 11, z: 12)

ResilientGlobalTestSuite.test("MyLargeGlobal") {
  expectEqual(9, myLargeGlobalUninitialized.w)
  expectEqual(10, myLargeGlobalUninitialized.x)
  expectEqual(11, myLargeGlobalUninitialized.y)
  expectEqual(12, myLargeGlobalUninitialized.z)
}

//
// Unknown size -- must call value witness functions for buffer
// management.
//

let myOtherGlobal = Size(w: 10, h: 15)

ResilientGlobalTestSuite.test("MyOtherGlobal") {
  expectEqual(10, myOtherGlobal.w)
  expectEqual(15, myOtherGlobal.h)
}

//
// Global variable is itself defined in a different module.
//

ResilientGlobalTestSuite.test("OtherGlobal") {
  expectEqual(1337, emptyGlobal.computed)
}

runAllTests()
