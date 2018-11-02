// RUN: %target-build-swift -sanitize=thread -target %sanitizers-target-triple %s -o %t_binary
// RUN: %env-TSAN_OPTIONS=ignore_interceptors_accesses=1:halt_on_error=1 %target-run %t_binary
// REQUIRES: executable_test
// REQUIRES: stress_test
// REQUIRES: tsan_runtime

// https://bugs.swift.org/browse/SR-6622
// XFAIL: linux

// Check that TSan does not report spurious races in witness table lookup.

func consume(_ x: Any) {}
protocol Q {
  associatedtype QA
  func deduceQA() -> QA
  static func foo()
}
extension Q {
  func deduceQA() -> Int { return 0 }
}
protocol Q2 {
  associatedtype Q2A
  func deduceQ2A() -> Q2A
}
extension Q2 {
  func deduceQ2A() -> Int { return 0 }
}
protocol P {
  associatedtype E : Q, Q2
}
struct B<T : Q> : Q, Q2 {
  static func foo() { consume(type(of: self)) }
}
struct A<T : Q> : P where T : Q2 {
  typealias E = B<T>
  let value: T
}
func foo<T : P>(_ t: T) {
  T.E.foo()
}
struct EasyType : Q, Q2 {
    static func foo() { consume(type(of: self)) }
}
extension Int : Q, Q2 {
  static func foo() { consume(type(of: self)) }
}

// Execute concurrently.
import StdlibUnittest
var RaceTestSuite = TestSuite("t")

RaceTestSuite.test("test_metadata") {
  runRaceTest(trials: 1) {
    foo(A<Int>(value: 5))
    foo(A<Int>(value: Int()))
    foo(A<EasyType>(value: EasyType()))
  }
}

runAllTests()
