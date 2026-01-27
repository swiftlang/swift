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

// A type on the client side that is never updated for the new world
struct Eagle: Bird {
  func eat(_ food: Int) -> Int { return food }
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
  let e = Eagle()
  let ans = libraryVersion() + clientVersion
  expectEqual(SomeType.feed(e, clientVersion), ans)
  expectEqual(SomeType.count(e, clientVersion), ans)

  #if !BEFORE
  let 💯 = Plus100Eagle()
  expectEqual(SomeType.feed(💯, clientVersion), ans + 100)
  expectEqual(SomeType.count(💯, clientVersion), ans + 100)
  #endif
}

ProtocolAddReparentedTest.test("Existential Types") {
  let e = Eagle()
  let ans = libraryVersion() + clientVersion
  expectEqual(ExistentialType.feed(e, clientVersion), ans)
  expectEqual(ExistentialType.count(e, clientVersion), ans)

  #if !BEFORE
  let 💯 = Plus100Eagle()
  expectEqual(ExistentialType.feed(💯, clientVersion), ans + 100)
  expectEqual(ExistentialType.count(💯, clientVersion), ans + 100)
  #endif
}

runAllTests()
