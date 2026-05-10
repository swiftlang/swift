// RUN: mkdir -p %t
// RUN: %target-clang %S/Inputs/NSSlowTaggedLocalizedString/NSSlowTaggedLocalizedString.m -fno-objc-arc -c -o %t/NSSlowTaggedLocalizedString.o
// RUN: %target-build-swift -g -parse-stdlib -Xfrontend -disable-access-control -I %S/Inputs/NSSlowTaggedLocalizedString/ %t/NSSlowTaggedLocalizedString.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: rdar100559801

// REQUIRES: executable_test
// REQUIRES: objc_interop

import NSSlowTaggedLocalizedString
import Swift

import StdlibUnittest

let longTaggedTests = TestSuite("NonContiguousTaggedStrings")
var constant = "Send Message to different Team"
//doesn't fit in a tagged pointer because of ', but does fit in a SmallString
var shortNonTagged = "Don\u{2019}t Save"

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

longTaggedTests.test("EqualNonASCIISubsetSmall") {
  if #available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) {
    if MemoryLayout<AnyObject>.size != 8 {
      return //no tagged pointers
    }
    
    var native = shortNonTagged.withUTF8 { String(decoding: $0, as: UTF8.self) }
    native.reserveCapacity(30) //force into non-small form so we can reverse bridge below
    let longTagged = NSSlowTaggedLocalizedString.createTest()!
    shortNonTagged.withCString {
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
}

runAllTests()

