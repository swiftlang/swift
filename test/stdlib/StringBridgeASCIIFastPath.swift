// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/String
// RUN: %target-codesign %t/String
// RUN: /bin/sh -c 'exec env %env-__CF_USER_TEXT_ENCODING="$(id -u):0:0" "$@"' -- %target-run %t/String
// RUN: /bin/sh -c 'exec env %env-__CF_USER_TEXT_ENCODING="$(id -u):1:14" "$@"' -- %target-run %t/String
// REQUIRES: executable_test, OS=macosx
// UNSUPPORTED: interpret, freestanding

import Foundation
import StdlibUnittest

var tests = TestSuite("StringBridgeASCIIFastPath")

tests.test("CF encoding") {
  // Verify that CF honors the requested environment setting.
  let setting = String(cString: getenv("__CF_USER_TEXT_ENCODING")!)
  let expected = UInt32(setting.split(separator: ":")[1])!
  expectEqual(expected, CFStringGetSystemEncoding())
}

tests.test("ASCII interior pointer") {
  for text in [
    "abcdefghijklmnop",
    "the quick fox jumped over the lazy brown dog",
    "abcdefghijklmnop\\qrst",
    "abcdefghijklmnop\0qrst",
  ] {
    let bytes = Array(text.utf8)
    let cocoa = bytes.withUnsafeBufferPointer {
      CFStringCreateWithBytes(
        nil, $0.baseAddress, $0.count,
        CFStringBuiltInEncodings.ASCII.rawValue, false)!
    }
    let string = cocoa as String
    expectTrue(string.isContiguousUTF8)
    expectEqualSequence(bytes, string.utf8)

    let (owner, pointer, count, usedScratch, allocated)
      : (AnyObject?, UnsafePointer<UInt8>, Int, Bool, Bool)
      = string._deconstructUTF8(scratch: nil)
    withExtendedLifetime(owner) {
      expectEqual(bytes.count, count)
      expectFalse(usedScratch)
      expectFalse(allocated)
      expectEqualSequence(
        bytes, UnsafeBufferPointer(start: pointer, count: count))
      let interior = CFStringGetCStringPtr(
        cocoa, CFStringBuiltInEncodings.ASCII.rawValue)!
      expectEqual(UnsafeRawPointer(interior), UnsafeRawPointer(pointer))
    }
  }
}

runAllTests()
