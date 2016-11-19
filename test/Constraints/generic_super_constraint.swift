// RUN: %target-swift-frontend %s -typecheck -verify

class Base<T> { }
class Derived: Base<Int> { }

func foo<T>(_ x: T) -> Derived where T: Base<Int>, T: Derived {
  return x
}

// FIXME: Should not be an error
// expected-error@+1{{cannot be a subclass of both 'Base<T>' and 'Derived'}}
func bar<T, U>(_ x: U, y: T) -> (Derived, Int) where U: Base<T>, U: Derived {
  // FIXME
  // expected-error@+1{{cannot convert return expression}}
  return (x, y)
}
