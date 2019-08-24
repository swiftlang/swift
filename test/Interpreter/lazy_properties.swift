// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var LazyPropertyTestSuite = TestSuite("LazyProperty")

var lazyPropertyInitialized = 0
var lazyPropertyInitialized2 = 0
var lazyPropertyInitialized3 = 0

func lazyInitFunction() -> Int {
  lazyPropertyInitialized += 1
  return 0
}

class LazyPropertyClass {
  var id : Int
  lazy var lazyProperty = lazyInitFunction()

  lazy var lazyProperty2: Int = {
    lazyPropertyInitialized2 += 1
    return 0
  }()

  lazy var lazyProperty3: Int! = {
    lazyPropertyInitialized3 += 1
    return 0
  }()

  init(_ ident : Int) {
    id = ident
  }
}

LazyPropertyTestSuite.test("Basic") {
  var a = LazyPropertyClass(1)

  expectEqual(0, lazyPropertyInitialized)
  _ = a.lazyProperty
  expectEqual(1, lazyPropertyInitialized)
  _ = a.lazyProperty

  a.lazyProperty = 42   // nothing interesting happens

  expectEqual(0, lazyPropertyInitialized2)
  _ = a.lazyProperty2
  expectEqual(1, lazyPropertyInitialized2)

  a = LazyPropertyClass(2)

  a = LazyPropertyClass(3)
  a.lazyProperty = 42
  expectEqual(1, lazyPropertyInitialized)

  expectEqual(0, lazyPropertyInitialized3)
  expectEqual(0, a.lazyProperty3)
  expectEqual(1, lazyPropertyInitialized3)

  a.lazyProperty3 = nil
  expectEqual(nil, a.lazyProperty3)
  expectEqual(1, lazyPropertyInitialized3)
}

// Swift 3 had a bogus 'property resetting' behavior,
// but we don't allow that anymore.

LazyPropertyTestSuite.test("Reset") {

}

runAllTests()
