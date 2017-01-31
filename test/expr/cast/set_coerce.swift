// RUN: %target-typecheck-verify-swift

class C : Hashable {
	var x = 0

  var hashValue: Int {
    return x
  }
}

func == (x: C, y: C) -> Bool { return true }


class D : C {}

var setC = Set<C>()
var setD = Set<D>()

// Test set upcasts
setC = setD
setD = setC // expected-error{{cannot assign value of type 'Set<C>' to type 'Set<D>'}}
