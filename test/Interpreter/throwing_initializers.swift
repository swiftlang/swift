// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

enum E : ErrorType {
  case X
}

func unwrap(b: Bool) throws -> Int {
  if b {
    throw E.X
  }
  return 0
}

class A {
  init(n: Int) {}

  init(b: Bool, n: Int) throws {
    if b {
      throw E.X
    }
  }

  convenience init(b1: Bool) throws {
    try unwrap(b1)
    self.init(n: 0)
  }

  convenience init(b2: Bool) throws {
    try self.init(n: unwrap(b2))
  }

  convenience init(b3: Bool) throws {
    self.init(n: 0)
    try unwrap(b3)
  }

  convenience init(b1: Bool, b2: Bool, b3: Bool, b4: Bool) throws {
    try unwrap(b1)
    try self.init(b: b2, n: unwrap(b3))
    try unwrap(b4)
  }
}

func mustThrow<T>(@autoclosure f: () throws -> T) {
  do {
    try f()
    preconditionFailure("Didn't throw")
  } catch {}
}

mustThrow(try A(b1: true))
mustThrow(try A(b2: true))
mustThrow(try A(b3: true))
mustThrow(try A(b1: true))
mustThrow(try A(b1: true, b2: false, b3: false, b4: false))
mustThrow(try A(b1: false, b2: true, b3: false, b4: false))
mustThrow(try A(b1: false, b2: false, b3: true, b4: false))
mustThrow(try A(b1: false, b2: false, b3: false, b4: true))

// CHECK: All OK
print("All OK")
