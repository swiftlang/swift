// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for error handling.
//

import StdlibUnittest


enum Excuse : Error { case CatAteHomework(LifetimeTracked) }

var ErrorHandlingTests = TestSuite("ErrorHandling")

func furball(_ b: Bool) throws -> LifetimeTracked {
  if b {
    throw Excuse.CatAteHomework(LifetimeTracked(0))
  } else {
    return LifetimeTracked(1)
  }
}

ErrorHandlingTests.test("tryCatch") {
  do {
    try expectEqual(furball(false), LifetimeTracked(1))
  } catch {
    expectUnreachable() 
  }

  do {
    try furball(true)
    expectUnreachable() 
  } catch let e {
    if case Excuse.CatAteHomework(let c) = e {
      expectEqual(c, LifetimeTracked(0))
    } else {
      expectUnreachable()
    }
  }
}

ErrorHandlingTests.test("tryOptional") {
  expectEqual(LifetimeTracked(1), try? furball(false))
  expectEqual(Optional<LifetimeTracked>.none, try? furball(true))
}

runAllTests()
