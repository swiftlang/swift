// RUN: %target-swift-frontend %s -typecheck -verify

class Base<T> { }
class Derived: Base<Int> { }

func foo<T>(_ x: T) -> Derived where T: Base<Int>, T: Derived {
	// expected-warning@-1{{redundant superclass constraint 'T' : 'Base<Int>'}}
	// expected-note@-2{{superclass constraint 'T' : 'Derived' written here}}
  return x
}

// FIXME: Should not be an error
// expected-error@+2{{generic parameter 'U' cannot be a subclass of both 'Derived' and 'Base<T>'}}
// expected-note@+1{{superclass constraint 'U' : 'Base<T>' written here}}
func bar<T, U>(_ x: U, y: T) -> (Derived, Int) where U: Base<T>, U: Derived {
  // FIXME
  // expected-error@+1{{cannot convert return expression}}
  return (x, y)
}
