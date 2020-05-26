// RUN: %target-typecheck-verify-swift -swift-version 4

protocol X {
  var foo: Int { get }
  var bar: Int { get }
}

class Y {
  var foo: Int = 0
}
extension Y {
  var bar: Int { return foo }
}

protocol Z : Y {
  var foo: Int { get }
  var bar: Int { get }
}

class GenericClass<T> {
  var foo: T
  init(_ foo: T) { self.foo = foo }
}
extension GenericClass {
  var bar: T { return foo }
}

// Make sure we keep all of the following cases unambiguous to retain source compatibility with Swift 4.1.

func testGenericPropertyProtocolClass<T : X & Y>(_ t: T) {
  _ = t.foo
  _ = t.bar
}

func testExistentialPropertyProtocolClass(_ t: X & Y) {
  _ = t.foo
  _ = t.bar
}

func testGenericPropertySubclassConstrainedProtocol<T : Z>(_ t: T) {
  _ = t.foo
  _ = t.bar
}

func testExistentialPropertySubclassConstrainedProtocol(_ t: Z) {
  _ = t.foo
  _ = t.bar
}

func testExistentialPropertyProtocolGenericClass(_ t: GenericClass<Int> & X) {
  _ = t.foo
  _ = t.bar
}

func testExistentialPropertyProtocolGenericClass(_ t: GenericClass<String> & X) {
  _ = t.foo
  _ = t.bar
}

extension X where Self : Y {
  func testGenericPropertyProtocolClass(_ x: Self) {
    _ = self.foo
    _ = self.bar
  }
}

extension X where Self : GenericClass<Int> {
  func testGenericPropertyProtocolGenericClass(_ x: Self) {
    _ = self.foo
    _ = self.bar
  }
}

extension X where Self : GenericClass<String> {
  func testGenericPropertyProtocolGenericClass(_ x: Self) {
    _ = self.foo
    _ = self.bar
  }
}
