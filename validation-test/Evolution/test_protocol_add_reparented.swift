// RUN: %target-resilience-test --skip-application-back-deploy --additional-compile-flags '-enable-experimental-feature Reparenting'
// REQUIRES: executable_test
// REQUIRES: swift_feature_Reparenting

import StdlibUnittest
import protocol_add_reparented


var ProtocolAddReparentedTest = TestSuite("ProtocolAddReparented")

#if BEFORE
let clientVersion = 0
#else
let clientVersion = 1
#endif

protocol P {
  func getPValue() -> Int
}

// A type on the client side that is never updated for the new world
struct Eagle: Bird {
  func eat(_ food: Int) -> Int { return food }
}

// A conditionally-conforming type that is not updated.
struct Lark<T> {
  let t: T
  init(_ t: T) { self.t = t }
}
extension Lark: Bird where T: P {
  func eat(_ food: Int) -> Int {
    return food + t.getPValue()
  }
}

// Strings return their length for a "P-value"
extension String: P {
  func getPValue() -> Int { return self.count }
}

#if !BEFORE
// A type on the client side that was updated to be aware of the new-world, providing its own witnesses.
struct Plus100Eagle: Bird {
  func eat(_ food: Int) -> Int { return food }

  public func eatSeed(_ food: Int) -> Int { return eat(food + libraryVersion() + 100) }
  public typealias Kind = UInt
  public var seedKinds: UInt {
    get { return UInt(libraryVersion() + 100) }
  }
}

extension UInt: @retroactive AsInt {
  public func asInt() -> Int { return Int(self) }
}
#endif

ProtocolAddReparentedTest.test("Some Types") {
  let base = libraryVersion() + clientVersion // basic answer
  do {
    let e = Eagle()
    expectEqual(SomeType.feed(e, clientVersion), base)
    expectEqual(SomeType.count(e, clientVersion), base)
  }

  do {
    let str = "12345"
    let e = Lark(str)
    expectEqual(SomeType.feed(e, clientVersion), base + str.count)
    expectEqual(SomeType.count(e, clientVersion), base)
  }

  #if !BEFORE
  do {
    let 💯 = Plus100Eagle()
    expectEqual(SomeType.feed(💯, clientVersion), base + 100)
    expectEqual(SomeType.count(💯, clientVersion), base + 100)
  }
  #endif
}

ProtocolAddReparentedTest.test("Existential Types") {
  let base = libraryVersion() + clientVersion // basic answer
  do {
    let e = Eagle()
    expectEqual(ExistentialType.feed(e, clientVersion), base)
    expectEqual(ExistentialType.count(e, clientVersion), base)
  }

  do {
    let str = "12345"
    let e = Lark(str)
    expectEqual(ExistentialType.feed(e, clientVersion), base + str.count)
    expectEqual(ExistentialType.count(e, clientVersion), base)
  }

  #if !BEFORE
  do {
    let 💯 = Plus100Eagle()
    expectEqual(ExistentialType.feed(💯, clientVersion), base + 100)
    expectEqual(ExistentialType.count(💯, clientVersion), base + 100)
  }
  #endif
}

runAllTests()
