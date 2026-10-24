// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// RUN: %target-build-swift -Onone %s -o %t/b.out
// RUN: %target-codesign %t/b.out
// RUN: %target-run %t/b.out

// REQUIRES: executable_test

import StdlibUnittest

var deinits = 0

@_nonatomicReferenceCounting
class Base {
  var value: Int
  var next: Base?
  init(_ value: Int) { self.value = value }
  deinit { deinits += 1 }
}

final class Derived: Base {
  var extra = [1, 2, 3]
}

final class Holder {
  var base: Base?
  weak var weakBase: Base?
  unowned var unownedBase: Base
  init(_ b: Base) { base = b; weakBase = b; unownedBase = b }
}

struct Wrapper { var b: Base; var d: Derived }

@inline(never) func identity<T>(_ t: T) -> T { t }

let suite = TestSuite("NonatomicClassRC")

suite.test("lifetime") {
  deinits = 0
  do {
    var head: Base? = nil
    for i in 0..<100 { let n = Base(i); n.next = head; head = n }
    var sum = 0
    var cur = head
    while let n = cur { sum += n.value; cur = n.next }
    expectEqual(4950, sum)
    head = nil
    cur = nil
  }
  expectEqual(100, deinits)
}

suite.test("subclass, struct, array, closure, existential") {
  deinits = 0
  do {
    let d = Derived(7)
    let w = Wrapper(b: Base(1), d: d)
    let copy = identity(w)
    let arr = [copy.b, copy.d, d]
    let closure = { arr.count + copy.d.value }
    expectEqual(10, closure())
    let any: AnyObject = identity(d)
    expectTrue(any === d)
  }
  expectEqual(2, deinits)
}

suite.test("weak and unowned") {
  deinits = 0
  var h: Holder? = Holder(Derived(3))
  expectEqual(3, h!.weakBase?.value)
  expectEqual(3, h!.unownedBase.value)
  let b = h!.base!
  h!.base = nil
  expectEqual(3, h!.weakBase?.value)
  h = nil
  expectEqual(0, deinits)
  _ = b
}

suite.test("uniqueness") {
  var b = Base(1)
  expectTrue(isKnownUniquelyReferenced(&b))
  let c = b
  withExtendedLifetime(c) { expectFalse(isKnownUniquelyReferenced(&b)) }
}

runAllTests()
