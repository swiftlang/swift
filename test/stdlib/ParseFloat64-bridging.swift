// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/a.out -enable-experimental-feature Extern
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Needed to declare the ABI entry point
// REQUIRES: swift_feature_Extern

import StdlibUnittest
import Foundation

let tests = TestSuite("FloatingPointParsing")

tests.test("Bridged - short") {
  let s1 = "1.02.03.0"
  let nss1 = NSString(utf8String: s1)!
  let bridged = String(nss1)
  let range = bridged.firstIndex(of: "2")!..<bridged.firstIndex(of: "3")!
  let sub = bridged[range]
  let parsed = Float64(sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}

tests.test("Bridged - long") {
  let s1 = "1.02.0000000000000000000000000000000000000000000000000000000000003.04.05.06.07.08.09.010.011.012.013.014.015.0"
  let nss1 = NSString(utf8String: s1)!
  let bridged = String(nss1)
  let range = bridged.firstIndex(of: "2")!..<bridged.firstIndex(of: "3")!
  let sub = bridged[range]
  let parsed = Float64(sub)
  expectNotNil(parsed)
  expectEqual(parsed!.bitPattern, (2.0).bitPattern)
}

runAllTests()
