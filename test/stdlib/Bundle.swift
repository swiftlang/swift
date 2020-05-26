// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var BundleTests = TestSuite("BundleTests")

BundleTests.test("Bundle.bundleForNilClass") {
  // Ensure that bundleForClass: tolerates a nil parameter. The
  // Foundation implementation does. The patched version from
  // ObjCRuntimeGetImageNameFromClass did not.
  //
  // SR-9188
  typealias BundleForClassFunc =
    @convention(c) (AnyObject, Selector, AnyObject?) -> Bundle
  
  let sel = #selector(Bundle.init(for:))
  let imp = unsafeBitCast(Bundle.method(for: sel), to: BundleForClassFunc.self)
  let bundleForNil = imp(Bundle.self, sel, nil);
  expectEqual(Bundle.main, bundleForNil)
}

runAllTests()
