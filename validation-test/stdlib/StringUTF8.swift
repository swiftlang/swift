// RUN: %empty-directory(%t)
// RUN: if [ %target-runtime == "objc" ]; \
// RUN: then \
// RUN:   %target-clang -fobjc-arc %S/Inputs/NSSlowString/NSSlowString.m -c -o %t/NSSlowString.o && \
// RUN:   %target-build-swift -I %S/Inputs/NSSlowString/ %t/NSSlowString.o %s -o %t/String; \
// RUN: else \
// RUN:   %target-build-swift %s -o %t/String; \
// RUN: fi

// RUN: %target-codesign %t/String
// RUN: %target-run %t/String
// REQUIRES: executable_test
// XFAIL: interpret

import StdlibUnittest
import StdlibCollectionUnittest

#if _runtime(_ObjC)
import NSSlowString
import Foundation  // For NSRange
#endif

extension String {
  func withFastUTF8IfAvailable<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R? {
    return try utf8.withContiguousStorageIfAvailable(f)
  }
  var isFastUTF8: Bool {
    return withFastUTF8IfAvailable({ _ in return 0 }) != nil
  }
  mutating func makeNative() { self += "" }

  var isASCII: Bool { return utf8.allSatisfy { $0 < 0x7f } }
}

var UTF8Tests = TestSuite("StringUTF8Tests")

var strings: Array<String> = [
  "abcd",
  "abcdefghijklmnop",
  "abcde\u{301}fghijk",
  "a\u{301}",
  "👻",
  "Spooky long string. 👻",
  "в чащах юга жил-был цитрус? да, но фальшивый экземпляр",
  "日",
]

let kCFStringEncodingASCII: UInt32 = 0x0600

UTF8Tests.test("Contiguous Access") {
  for string in strings {
    print(string)

    // Native strings are contiguous UTF-8
    expectTrue(string.isFastUTF8)
    expectEqualSequence(
      Array(string.utf8), string.withFastUTF8IfAvailable(Array.init)!)

    // FIXME: Bridge small non-ASCII as StringStorage
    // expectTrue(((string as NSString) as String).isFastUTF8)

    var copy = string
    expectTrue(copy.isFastUTF8)
    copy.makeNative()
    expectTrue(copy.isFastUTF8)

    // FIXME: Bridge small non-ASCII as StringStorage
    // expectTrue(((copy as NSString) as String).isFastUTF8)

#if _runtime(_ObjC)
    // Lazily bridged strings are not contiguous UTF-8
    var slowString = NSSlowString(string: string) as String
    expectFalse(slowString.isFastUTF8)
    expectEqualSequence(string.utf8, slowString.utf8)

    // They become fast when mutated
    slowString.makeNative()
    expectTrue(slowString.isFastUTF8)
    expectEqualSequence(
      string.utf8, slowString.withFastUTF8IfAvailable(Array.init)!)

    // Contiguous ASCII CFStrings provide access, even if lazily bridged
    if string.isASCII {
      let cfString = string.withCString {
        CFStringCreateWithCString(nil, $0, kCFStringEncodingASCII)!
      } as String
      expectTrue(cfString.isFastUTF8)
      expectEqualSequence(
        string.utf8, cfString.withFastUTF8IfAvailable(Array.init)!)
    }
#endif
  }
}

runAllTests()

