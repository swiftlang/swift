// RUN: mkdir -p %t
// RUN: %target-clang %S/Inputs/NSSlowTaggedLocalizedString/NSSlowTaggedLocalizedString.m -fno-objc-arc -c -o %t/NSSlowTaggedLocalizedString.o
// RUN: %target-build-swift -g -parse-stdlib -Xfrontend -disable-access-control -I %S/Inputs/NSSlowTaggedLocalizedString/ %t/NSSlowTaggedLocalizedString.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import NSSlowTaggedLocalizedString
import Swift

import StdlibUnittest

let longTaggedTests = TestSuite("NonContiguousTaggedStrings")
var constant = "Send Message to different Team"

func runEqualLongTagged() {
  
  if MemoryLayout<AnyObject>.size != 8 {
    return //no tagged pointers
  }
  
  let native = constant.withUTF8 { String(decoding: $0, as: UTF8.self) }
  let longTagged = NSSlowTaggedLocalizedString.createTest()!
  constant.withCString {
    NSSlowTaggedLocalizedString.setContents($0)
  }
  defer {
    NSSlowTaggedLocalizedString.setContents(nil)
  }
  let reverseBridged = unsafeBitCast(native._guts._object.largeAddressBits, to: AnyObject.self)
  let eq = reverseBridged.isEqual(to: longTagged)
  expectEqual(eq, 1)
  _fixLifetime(native)
}

longTaggedTests.test("EqualLongTagged") {
  runEqualLongTagged()
}

runAllTests()

