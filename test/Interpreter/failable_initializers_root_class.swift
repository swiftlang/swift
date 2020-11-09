// RUN: %target-run-simple-swift

// REQUIRES: executable_test

import StdlibUnittest


var FailableInitTestSuite = TestSuite("FailableInit")

var deinitCalled = 0

func mustFail<T>(f: () -> T?) {
  if f() != nil {
    preconditionFailure("Didn't fail")
  }
}

func mustSucceed<T>(f: () -> T?) {
  if f() == nil {
    preconditionFailure("Didn't succeed")
  }
}

class FirstClass {
  var x: LifetimeTracked

  init?(n: Int) {
    if n == 0 {
      return nil
    }

    x = LifetimeTracked(0)

    if n == 1 {
      return nil
    }
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("FirstClass") {
  deinitCalled = 0

  mustFail { FirstClass(n: 0) }
  expectEqual(0, deinitCalled)

  mustFail { FirstClass(n: 1) }
  expectEqual(1, deinitCalled)

  mustSucceed { FirstClass(n: 2) }
  expectEqual(2, deinitCalled)
}

class FirstClassTrivial {
  var x: Int

  init?(n: Int) {
    if n == 0 {
      return nil
    }

    x = 0

    if n == 1 {
      return nil
    }
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("FirstClassTrivial") {
  deinitCalled = 0

  mustFail { FirstClassTrivial(n: 0) }
  expectEqual(0, deinitCalled)

  mustFail { FirstClassTrivial(n: 1) }
  expectEqual(1, deinitCalled)

  mustSucceed { FirstClassTrivial(n: 2) }
  expectEqual(2, deinitCalled)
}

class SecondClass {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init?(n: Int) {
    if n == 0 {
      return nil
    }

    x = LifetimeTracked(0)

    if n == 1 {
      return nil
    }

    y = LifetimeTracked(0)

    if n == 2 {
      return nil
    }
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("SecondClass") {
  deinitCalled = 0

  mustFail { SecondClass(n: 0) }
  expectEqual(0, deinitCalled)

  mustFail { SecondClass(n: 1) }
  expectEqual(0, deinitCalled)

  mustFail { SecondClass(n: 2) }
  expectEqual(1, deinitCalled)

  mustSucceed { SecondClass(n: 3) }
  expectEqual(2, deinitCalled)
}

class SecondClassTrivial {
  var x: Int
  var y: Int

  init?(n: Int) {
    if n == 0 {
      return nil
    }

    x = 0

    if n == 1 {
      return nil
    }

    y = 0

    if n == 2 {
      return nil
    }
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("SecondClassTrivial") {
  deinitCalled = 0

  mustFail { SecondClassTrivial(n: 0) }
  expectEqual(0, deinitCalled)

  mustFail { SecondClassTrivial(n: 1) }
  expectEqual(0, deinitCalled)

  mustFail { SecondClassTrivial(n: 2) }
  expectEqual(1, deinitCalled)

  mustSucceed { SecondClassTrivial(n: 3) }
  expectEqual(2, deinitCalled)
}

runAllTests()
