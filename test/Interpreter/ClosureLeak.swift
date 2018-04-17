// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest

struct Large {
  var a : LifetimeTracked = LifetimeTracked(0)
  var b : LifetimeTracked = LifetimeTracked(0)
  var c : LifetimeTracked = LifetimeTracked(0)
  var d : LifetimeTracked = LifetimeTracked(0)
  var e : LifetimeTracked = LifetimeTracked(0)
  var f : LifetimeTracked = LifetimeTracked(0)
}

func doit( action: () -> ()) {
  action()
}

func foo(_ s: Large) {
    doit {
      let _ = s
    }
 }

var Tests = TestSuite("ClosureLeak")


Tests.test("dontLeak") {
  do {
    let s = Large()
    foo(s)
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()
