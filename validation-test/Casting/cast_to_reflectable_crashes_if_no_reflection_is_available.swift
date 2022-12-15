// RUN: %target-run-simple-swift(-enable-upcoming-feature OptInReflection -target %target-cpu-apple-macosx99.99) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// REQUIRES: executable_test

import Foundation
import StdlibUnittest

// At the end of the file, run all of the tests.
var Tests = TestSuite("Reflectable casting")
defer {
  runAllTests()
}

public protocol Foo {}

public struct Bar: Foo {
	let a: Int
	let b: String
}

let bar = Bar(a: 999, b: "bar")

Tests.test("Reflectable Cast") {
  do {
    let bar1 = bar as? Reflectable
    expectNil(bar1)

    let bar2 = bar as? Reflectable????
    expectNil(bar2)

    let bar3 = bar as? Reflectable & Foo
    expectNil(bar3)

    let bar4 = bar as? (Reflectable & Foo)????
    expectNil(bar4)
  }
  
  // CHECK-LABEL: [ RUN      ] Reflectable casting.Reflectable Cast
  // CHECK: stderr>>> Could not cast value of type 'main.Bar' ({{.*}}) to 'Reflectable', reflection metadata isn't available at runtime for this type.
  // CHECK: stderr>>> OK: saw expected "crashed: sigabrt"
  // CHECK: [       OK ] Reflectable casting.Reflectable Cast
  expectCrashLater()
  do {
    debugPrint(bar as! Reflectable & Foo)
  }
}
