// RUN: %target-typecheck-verify-swift %s -warn-redundant-requirements

class Base<T> { }
class Derived: Base<Int> { }

// expected-warning@+1 {{redundant superclass constraint 'T' : 'Base<Int>'}}
func foo<T>(_ x: T) -> Derived where T: Base<Int>, T: Derived {
  return x
}

// FIXME: There is no explicit same-type requirement written.
// expected-warning@+1{{same-type requirement makes generic parameter 'T' non-generic}}
func bar<T, U>(_ x: U, y: T) -> (Derived, Int) where U: Base<T>, U: Derived {
  return (x, y)
}

// SR-7551 captures a crash on this code.
class IntegerClass : ExpressibleByIntegerLiteral, Equatable {
  required init(integerLiteral value: Int) { }
  static func ==(lhs: IntegerClass, rhs: IntegerClass) -> Bool { return true }
}

func foo<T: IntegerClass>(_ num: T) { let _ =  num != 0 }
