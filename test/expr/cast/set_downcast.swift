// RUN: %target-typecheck-verify-swift

class C : Hashable {
	var x = 0

  var hashValue: Int {
    return x
  }
}

func == (x: C, y: C) -> Bool { return true }


class D : C {}

// Unrelated to the classes above.
class U : Hashable { 
  var hashValue: Int {
    return 0
  }
}

func == (x: U, y: U) -> Bool { return true }

var setC = Set<C>()
var setD = Set<D>()

// Test set forced downcasts
setD = setC as! Set<D>

// Test set conditional downcasts
if let _ = setC as? Set<D> { }

// Test set downcasts to unrelated types.
_ = setC as! Set<U> // expected-warning{{cast from 'Set<C>' to unrelated type 'Set<U>' always fails}}

// Test set conditional downcasts to unrelated types
if let _ = setC as? Set<U> { } // expected-warning{{cast from 'Set<C>' to unrelated type 'Set<U>' always fails}}
