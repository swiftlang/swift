// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-Xfrontend -enable-lifetime-resolution)

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

class ConditionalClass {
  var x: LifetimeTracked
  var y: Int

  init?(initX: Bool, fail: Bool) {
    y = 0

    if initX {
      x = LifetimeTracked(0)
    }

    if fail {
      return nil
    }

    x = LifetimeTracked(1)
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("ConditionalClass") {
  deinitCalled = 0

  mustFail { ConditionalClass(initX: false, fail: true) }
  expectEqual(0, deinitCalled)

  mustFail { ConditionalClass(initX: true, fail: true) }
  expectEqual(1, deinitCalled)

  mustSucceed { ConditionalClass(initX: true, fail: false) }
  expectEqual(2, deinitCalled)

  mustSucceed { ConditionalClass(initX: false, fail: false) }
  expectEqual(3, deinitCalled)
}

class TwoConditionalClass {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init?(initX: Bool, initY: Bool) {
    if initX {
      x = LifetimeTracked(0)
    }

    if initY {
      y = LifetimeTracked(1)
    }

    if !initX || !initY {
      return nil
    }

    x = LifetimeTracked(2)
    y = LifetimeTracked(3)
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("TwoConditionalClass") {
  deinitCalled = 0

  mustFail { TwoConditionalClass(initX: false, initY: false) }
  mustFail { TwoConditionalClass(initX: true, initY: false) }
  mustFail { TwoConditionalClass(initX: false, initY: true) }
  expectEqual(0, deinitCalled)

  mustSucceed { TwoConditionalClass(initX: true, initY: true) }
  expectEqual(1, deinitCalled)
}

struct InitError: Error {}

class ThrowingClass {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init(fail: Bool) throws {
    x = LifetimeTracked(0)

    if fail {
      throw InitError()
    }

    y = LifetimeTracked(1)
  }

  deinit {
    deinitCalled += 1
  }
}

FailableInitTestSuite.test("ThrowingClass") {
  deinitCalled = 0

  mustFail { try? ThrowingClass(fail: true) }
  expectEqual(0, deinitCalled)

  mustSucceed { try? ThrowingClass(fail: false) }
  expectEqual(1, deinitCalled)
}

runAllTests()
