// RUN: %target-run-simple-swift

// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var CFTestSuite = TestSuite("CoreFoundation")

extension CFString {
  static func from(_ contents: String) -> CFString {
    return CFStringCreateWithCString(nil, contents, /*ascii*/0)
  }
}

CFTestSuite.test("Set<CFString>") {
  var s = Set<CFString>()
  // Use long strings to avoid the tagged pointer optimization.
  s.insert(.from("abcxxxxxxxxxxx"))
  s.insert(.from("defxxxxxxxxxxx"))
  expectTrue(s.contains(.from("abcxxxxxxxxxxx")))
  expectFalse(s.contains(.from("efxxxxxxxxxxx")))
  expectTrue(s.contains("abcxxxxxxxxxxx" as NSString))
  expectFalse(s.contains("efxxxxxxxxxxx" as NSString))
  // Attempt to make this really a Swift string that's then bridged.
  let good = "abcxxxxxxxxxxx"
  expectTrue(s.contains(good as NSString))
  let bad = "efxxxxxxxxxxx"
  expectFalse(s.contains(bad as NSString))
}

CFTestSuite.test("AnyHashable") {
  let strings = ["abc" as NSString as AnyHashable, "def" as CFString as AnyHashable]
  expectTrue(strings.contains("abc"))
  expectTrue(strings.contains("def"))

  let stringSet = Set(strings)
  expectTrue(stringSet.contains("abc"))
  expectTrue(stringSet.contains("def"))
}

CFTestSuite.test("Dictionary/casting") {
  let orig: [CFString: Any] = [
    .from("abcxxxxxxxxxxx"): "abc",
    .from("defxxxxxxxxxxx"): "def"
  ]
  expectEqual(orig[.from("abcxxxxxxxxxxx")] as! String?, "abc")

  let bridged = orig as [String: Any]
  expectEqual(bridged["abcxxxxxxxxxxx"] as! String?, "abc")

  let upcast = orig as [AnyHashable: Any]
  expectEqual(upcast["abcxxxxxxxxxxx"] as! String?, "abc")
}

CFTestSuite.test("Dictionary/as CFDictionary") {
  let orig: [CFString: Any] = [
    .from("abcxxxxxxxxxxx"): "abc",
    .from("defxxxxxxxxxxx"): "def"
  ]
  expectEqual(orig[.from("abcxxxxxxxxxxx")] as! String?, "abc")

  let cf = orig as CFDictionary
  withExtendedLifetime(CFString.from("abcxxxxxxxxxxx")) {
    expectTrue(CFDictionaryContainsKey(cf, Unmanaged.passUnretained($0).toOpaque()))
  }
}

CFTestSuite.test("Dictionary/round-trip") {
  let orig: [CFString: Any] = [
    .from("abcxxxxxxxxxxx"): "abc",
    .from("defxxxxxxxxxxx"): "def"
  ]
  expectEqual(orig[.from("abcxxxxxxxxxxx")] as! String?, "abc")

  let cf = orig as CFDictionary

  // This is an unchecked cast because we can't check the types of CF objects.
  let swiftTyped = cf as! [CFString: Any]
  expectEqual(swiftTyped[.from("abcxxxxxxxxxxx")] as! String?, "abc")

  let swiftBridged = cf as? [String: Any]
  expectNotNil(swiftBridged)
  expectEqual(swiftBridged!["abcxxxxxxxxxxx"] as! String?, "abc")

  // FIXME: CF-to-AnyHashable isn't permitted yet, so we need 'as?'.
  let swiftAny = cf as? [AnyHashable: Any]
  expectNotNil(swiftAny)
  expectEqual(swiftAny!["abcxxxxxxxxxxx"] as! String?, "abc")
}


runAllTests()
