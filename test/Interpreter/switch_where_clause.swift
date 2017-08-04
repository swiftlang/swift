// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

func boo(_: LifetimeTracked) -> Bool { return true }

var tests = TestSuite("switches with where clauses")

enum Foo {
  case a(LifetimeTracked)
  case b(LifetimeTracked)
}

func foo(_ x: Foo, _ y: Foo, _ condition: (LifetimeTracked) -> Bool) -> Bool {
     switch (x, y) {
     case (.a(let xml), _),
          (_, .a(let xml)) where condition(xml):
          return true
     default:
        return false
     }
}

tests.test("all paths through a switch with guard") {
  _ = foo(.a(LifetimeTracked(0)), .a(LifetimeTracked(1)), { _ in true })
  _ = foo(.a(LifetimeTracked(2)), .b(LifetimeTracked(3)), { _ in true })
  _ = foo(.b(LifetimeTracked(4)), .a(LifetimeTracked(5)), { _ in true })
  _ = foo(.b(LifetimeTracked(6)), .b(LifetimeTracked(7)), { _ in true })

  _ = foo(.a(LifetimeTracked(10)), .a(LifetimeTracked(11)), { _ in false })
  _ = foo(.a(LifetimeTracked(12)), .b(LifetimeTracked(13)), { _ in false })
  _ = foo(.b(LifetimeTracked(14)), .a(LifetimeTracked(15)), { _ in false })
  _ = foo(.b(LifetimeTracked(16)), .b(LifetimeTracked(17)), { _ in false })
}

runAllTests()
