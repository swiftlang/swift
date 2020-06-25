import StdlibUnittest

import module1

var Tests = TestSuite("CrossModuleDerivativeAttr")

Tests.test("CrossFile") {
  let grad = gradient(at: 0, in: fCrossFile)
  expectEqual(10, grad)
}

runAllTests()
