// RUN: %target-swift-frontend %s -parse -verify

class Base<T> { }
class Derived: Base<Int> { }

func foo<T where T: Base<Int>, T: Derived>(_ x: T) -> Derived {
  return x
}

// FIXME: Should not be an error
// expected-error@+1{{cannot be a subclass of both 'Base<T>' and 'Derived'}}
func bar<T, U where U: Base<T>, U: Derived>(_ x: U, y: T) -> (Derived, Int) {
  // FIXME
  // expected-error@+1{{cannot convert return expression}}
  return (x, y)
}
