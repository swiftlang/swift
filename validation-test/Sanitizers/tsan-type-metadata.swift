// REQUIRES: rdar64809726
// RUN: %target-swiftc_driver -sanitize=thread %import-libdispatch %s -o %t_binary
// RUN: %env-TSAN_OPTIONS=halt_on_error=1 %target-run %t_binary
// REQUIRES: executable_test
// REQUIRES: stress_test
// REQUIRES: tsan_runtime

// We expect not to report any races on this testcase.

// This test exercises accesses to type metadata, which uses lockless
// synchronization in the runtime that is relied upon by the direct accesses in the IR.
// We have to make sure TSan does not see the accesses to the metadata from the IR.
// Otherwise, it will report a race.

// Generic classes.
private class KeyWrapper<T: Hashable> {
  let value: T

  init(_ value: T) {
    self.value = value
  }
  func present() {
    print("Key: \(value)")
  }
}
private class ValueWrapper<T> {
  let value: T
  init(_ value: T) {
    self.value = value
  }
  func present() {
    print("Value: \(value)")
  }
}

// Concrete a class that inherits a generic base.
class Base<T> {
  var first, second: T
  required init(x: T) {
    first = x
    second = x
  }
  func present() {
    print("\(type(of: self)) \(T.self) \(first) \(second)")
  }
}
class SuperDerived: Derived {
}
class Derived: Base<String> {
  var third: String
  required init(x: String) {
    third = x
    super.init(x: x)
  }
  override func present() {
    super.present()
    print("...and \(third)")
  }
}
func presentBase<T>(_ base: Base<T>) {
  base.present()
}
func presentDerived(_ derived: Derived) {
  derived.present()
}

public func testMetadata<Key: Hashable, Value>(_ key: Key, _ value: Value) {
  let wrappedKey = KeyWrapper(key)
  wrappedKey.present()
  ValueWrapper(value).present()
  presentBase(SuperDerived(x: "two"))
  presentBase(Derived(x: "two"))
  presentBase(Base(x: "two"))
  presentBase(Base(x: 2))
  presentDerived(Derived(x: "two"))
}

// Execute concurrently.
import StdlibUnittest
var RaceTestSuite = TestSuite("t")

RaceTestSuite.test("test_metadata") {
  runRaceTest(trials: 1) {
    testMetadata(4, 4)
  }
}

runAllTests()
