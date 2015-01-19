// RUN: %target-parse-verify-swift

class C : Hashable {
	var x = 0

  var hashValue: Int {
    return x
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
dictCD = dictCC // expected-error{{cannot assign a value of type 'Dictionary<C, C>' to a value of type 'Dictionary<C, D>'}}


dictDC = dictDD
dictDC = dictCD // expected-error{{cannot assign a value of type 'Dictionary<C, D>' to a value of type 'Dictionary<D, C>'}}

dictDD = dictCC // expected-error{{cannot assign a value of type 'Dictionary<C, C>' to a value of type 'Dictionary<D, D>'}}

