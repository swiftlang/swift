// RUN: mkdir -p %t
// RUN: %target-clang -fobjc-arc %S/Inputs/NSSlowTaggedLocalizedString/NSSlowTaggedLocalizedString.m -fno-objc-arc -c -o %t/NSSlowTaggedLocalizedString.o
// RUN: %target-build-swift -Xfrontend -disable-access-control -I %S/Inputs/NSSlowTaggedLocalizedString/ %t/NSSlowTaggedLocalizedString.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import NSSlowTaggedLocalizedString
import Swift

import StdlibUnittest

let longTaggedTests = TestSuite("NonContiguousTaggedStrings")

longTaggedTests.test("EqualLongTagged") {
  var native = "Send Message to different Team"
  let longTagged = NSSlowTaggedLocalizedString.createTestString()
  NSSlowTaggedLocalizedString.setContents(&native)
  defer {
    NSSlowTaggedLocalizedString.setContents(nil)
  }
  let reverseBridged = native._guts._object.objCBridgeableObject
  expectEqual(
    reverseBridged.isEqual(to: longTagged),
    longTagged.isEqual(to: reverseBridged)
  )
}

runAllTests()

