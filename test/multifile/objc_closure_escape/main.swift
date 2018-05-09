// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-objc-header %S/../Inputs/objc-escape/DangerousEscaper.swift -emit-objc-header-path %t/DangerousEscaper.h
// RUN: %target-clang -c %S/../Inputs/objc-escape/Escaper.m -fobjc-arc -I %t -o %t/Escaper.o
// RUN: %target-swiftc_driver -import-objc-header %S/../Inputs/objc-escape/Escaper.h %S/../Inputs/objc-escape/DangerousEscaper.swift %s %t/Escaper.o -o %t/TestObjcProto
// RUN: %target-run %t/TestObjcProto 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import Dispatch
import StdlibUnittest


var testSuite = TestSuite("ObjectiveCClosureEscape")


public func couldActuallyEscape(_ closure: @escaping () -> (), _ villian: DangerousEscaper) {
  villian.mightBeNaughty(closure)
}

class Harmless : DangerousEscaper {
  @objc
  func mightBeNaughty(_ mayActuallyEscape: () -> ()) {
    mayActuallyEscape()
  }
}

class Printer {
  func printIt() {
    print("Printer")
  }
}

public func test() {
  var x = Printer()
  couldActuallyEscape( {x.printIt() } , Harmless())

  var y = Escaper()
  couldActuallyEscape( {x.printIt() } , y as! DangerousEscaper)
}

testSuite.test("testEscaping") {
// CHECK: closure argument passed as @noescape to Objective-C has escaped: file {{.*}}main.swift, line 19, column 26
  expectCrashLater()
  test()
}

runAllTests()
