// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -o %t/main -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

func foo(_ x: @escaping () -> ()) { x() }

public class C {
  var x: Int = 0

  init(blah: ()) {}

  @objc convenience init() {
    self.init(blah: ())

    foo { [weak self] in
      guard let `self` = self else { return }
      self.x += 1
    }
  }
}

var ConvenienceInitSelfTest = TestSuite("ConvenienceInitSelf")

ConvenienceInitSelfTest.test("SelfMetadata") {
  let c = C()
  expectEqual(c.x, 1)
}

runAllTests()