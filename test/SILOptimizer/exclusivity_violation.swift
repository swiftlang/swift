// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)

// REQUIRES: executable_test

// For some reason we don't get exclusivity violations on older OSes
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest


var tests = TestSuite("exclusivity checking")

struct NC: ~Copyable {
  var i: Int = 1

  mutating func add(_ other: borrowing Self) {
    i += other.i
    i += other.i
    print(self.i, other.i)
  }
}

class C1 {
  var nc = NC()

  func foo() {
    nc.add(nc)
  }
}

struct S {
  var i: Int = 1

  mutating func add(_ c: C2) {
    let other = c.getS()
    i += other.i
    i += other.i
    print(self.i, other.i)
  }
}

final class C2 {
  var s = S()

  @inline(never)
  func getS() -> S { s }

  func foo() {
    s.add(self)
  }
}

tests.test("non-copyable type")
  .crashOutputMatches("Simultaneous accesses")
  .code {
    expectCrashLater()

    C1().foo()
  }

tests.test("copyable type")
  .crashOutputMatches("Simultaneous accesses")
  .code {
    expectCrashLater()

    C2().foo()
  }

runAllTests()
