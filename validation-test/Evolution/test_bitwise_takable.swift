// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import bitwise_takable


var BitwiseTakable = TestSuite("BitwiseTakable")

BitwiseTakable.test("test") {
  let c = createContainerReporter()
  expectEqual("Container(2)", c.report())
  let p = createPairContainerReporter()
  expectEqual("PairContainer(Container(3), Container(4))", p.report())
  let e = createEnumContainerReporter()
  expectEqual("EnumContainer(Container(5))", e.report())

  let r : Reporter = Container(s2)
  expectEqual("Container(2)", report(r))

  let r2 : Reporter = PairContainer((Container(s3), Container(s4)))
  expectEqual("PairContainer(Container(3), Container(4))", report(r2))

  let r3 : Reporter = EnumContainer.Some(Container(s5))
  expectEqual("EnumContainer(Container(5))", report(r3))

}

runAllTests()
