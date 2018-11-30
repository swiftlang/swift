// RUN: %target-typecheck-verify-swift

class C : Hashable {
	var x = 0

  func hash(into hasher: inout Hasher) {
    hasher.combine(x)
  }
}

func == (x: C, y: C) -> Bool { return true }


class D : C {}

// Test dictionary upcasts
var dictCC = Dictionary<C, C>()
var dictCD = Dictionary<C, D>()
var dictDC = Dictionary<D, C>()
var dictDD = Dictionary<D, D>()

dictCC = dictCD
dictCC = dictDC
dictCC = dictDD

dictCD = dictDD
dictCD = dictCC // expected-error{{cannot assign value of type '[C : C]' to type '[C : D]'}}


dictDC = dictDD
dictDC = dictCD // expected-error{{cannot assign value of type '[C : D]' to type '[D : C]'}}

dictDD = dictCC // expected-error{{cannot assign value of type '[C : C]' to type '[D : D]'}}

