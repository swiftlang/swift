// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var StringBridgeTests = TestSuite("StringBridgeTests")

extension String {
  init(fromCocoa s: String) {
    self = (s as NSString) as String
  }


}

func expectSmall(_ str: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
 ) {
  switch str._classify()._form {
    case ._small: return
    default: expectationFailure("expected: small", trace: "",
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}
func expectCocoa(_ str: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
 ) {
  switch str._classify()._form {
    case ._cocoa: return
    default: expectationFailure("expected: cocoa", trace: "",
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}

StringBridgeTests.test("Tagged NSString") {
  guard #available(macOS 10.13, iOS 11.0, tvOS 11.0, *) else { return }
#if arch(i386) || arch(arm)
#else
  // Bridge tagged strings as small
  expectSmall((("0123456" as NSString) as String))
  expectSmall((("012345678" as NSString) as String))
  expectSmall((("aaaaaaaaaaa" as NSString) as String))
  expectSmall((("bbbbbbbbb" as NSString) as String))

  // Bridge non-tagged as non-small even if they fit, for fear of losing
  // associated information
  let bigAs = ("aaaaaaaaaaaa" as NSString) as String
  let bigBs = ("bbbbbbbbbb" as NSString) as String
  let bigQs = ("????????" as NSString) as String
  expectCocoa(bigAs)
  expectCocoa(bigBs)
  expectCocoa(bigQs)

#if false // TODO(SR-7594): re-enable
  let littleAsNSString = ("aa" as NSString)
  var littleAs = littleAsNSString as String

  // But become small when appended to
  expectSmall(bigAs + "c")
  expectSmall(bigBs + "c")
  expectSmall("a\(bigAs)")
  expectSmall("a\(bigBs)")
  expectSmall(littleAs + bigQs)
  expectSmall(bigQs + littleAs)
  expectSmall("\(littleAs)bigQs\(littleAs)")
#endif // false

#endif // not 32bit
}

runAllTests()

