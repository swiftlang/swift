// RUN: %target-run-simple-swift(-O -Xfrontend -enable-opt-in-reflection-metadata) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// REQUIRES: executable_test

import Foundation
import StdlibUnittest

// At the end of the file, run all of the tests.
var Tests = TestSuite("Reflectable casting")
defer {
  runAllTests()
}

public struct Bar {
	let a: Int
	let b: String
}

let bar = Bar(a: 999, b: "bar")

Tests.test("Forced Cast to Reflectable") {
  do {
    let _bar = bar as? Reflectable
    expectNil(_bar)
  }
  
  // CHECK-LABEL: [ RUN      ] Reflectable casting.Forced Cast to Reflectable
  // CHECK: stderr>>> Could not cast value of type 'main.Bar' ({{.*}}) to 'Reflectable', reflection metadata isn't available at runtime for this type.
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] Reflectable casting.Forced Cast to Reflectable
  expectCrashLater()
  do {
    let _bar = bar as! Reflectable
    debugPrint(_bar)
  }
}
