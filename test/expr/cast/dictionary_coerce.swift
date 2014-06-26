// RUN: %swift -parse %s -verify

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
dictCD = dictCC // expected-error{{'C' is not identical to 'D'}}


dictDC = dictDD
dictDC = dictCD // expected-error{{'C' is not identical to 'D'}}

dictDD = dictCC // expected-error{{'C' is not identical to 'D'}}

