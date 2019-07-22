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

class FakeString : NSString {
  var contents: NSString
  
  override init() {
    contents = NSString()
    super.init()
  }
  
  required init?(coder aDecoder: NSCoder) {
    guard let guts = NSString(coder: aDecoder) else {
      return nil
    }
    contents = guts
    super.init()
  }
  
  init(str: String) {
    contents = str as NSString
    super.init()
  }
  
  override var length: Int {
    return contents.length
  }
  
  override func character(at index: Int) -> unichar {
    return contents.character(at: index)
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

  // Bridge non-__NSCFString as non-small even if they fit, for fear of losing
  // associated information
  let subclassAs = FakeString(str: "aaaaaaaaaaaa") as String
  let subclassBs = FakeString(str: "bbbbbbbbbb") as String
  let subclassQs = FakeString(str: "????????") as String
  expectCocoa(subclassAs)
  expectCocoa(subclassBs)
  expectCocoa(subclassQs)
  
  // Bridge regular NSStrings (__NSCFString) as small if they fit
  let bigAs = ("aaaaaaaaaaaa" as NSString) as String
  let bigBs = ("bbbbbbbbbb" as NSString) as String
  let bigQs = ("????????" as NSString) as String
  expectSmall(bigAs)
  expectSmall(bigBs)
  expectSmall(bigQs)

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

func returnOne<T>(_ t: T) -> Int { return 1 }
StringBridgeTests.test("Character from NSString") {
  guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }

  // NOTE: Using hard-coded literals to directly construct NSStrings
  let ns1 = "A" as NSString
  let ns2 = "A\u{301}" as NSString
  let ns3 = "𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ" as NSString
  let ns4 = FakeString(str: "𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ")

  let c1 = Character(ns1 as String)
  let c2 = Character(ns2 as String)
  let c3 = Character(ns3 as String)
  let c4 = Character(ns4 as String)

  expectEqual("A", String(c1))
  expectNotNil(String(c1).utf8.withContiguousStorageIfAvailable(returnOne))

  expectEqual("A\u{301}", String(c2))
  expectNotNil(String(c2).utf8.withContiguousStorageIfAvailable(returnOne))
  expectNotNil((ns2 as String).utf8.withContiguousStorageIfAvailable(returnOne))

  expectEqual("𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ", String(c3))
  expectNotNil(String(c3).utf8.withContiguousStorageIfAvailable(returnOne))
  expectNil((ns3 as String).utf8.withContiguousStorageIfAvailable(returnOne))
  
  expectEqual("𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ", String(c4))
  expectNotNil(String(c4).utf8.withContiguousStorageIfAvailable(returnOne))
  expectNil((ns4 as String).utf8.withContiguousStorageIfAvailable(returnOne))
}


runAllTests()

