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
dictCD = dictCC // expected-error{{cannot convert value of type '[C : C]' to '[C : D]' in assignment, arguments to generic parameter 'Value' ('C' and 'D') are expected to be equal}}


dictDC = dictDD
dictDC = dictCD // expected-error {{cannot convert value of type '[C : D]' to '[D : C]' in assignment, arguments to generic parameter 'Key' ('C' and 'D') are expected to be equal}}
// expected-error@-1 {{cannot convert value of type '[C : D]' to '[D : C]' in assignment, arguments to generic parameter 'Value' ('D' and 'C') are expected to be equal}}

dictDD = dictCC // expected-error{{cannot convert value of type '[C : C]' to '[D : D]' in assignment, arguments to generic parameter 'Key' ('C' and 'D') are expected to be equal}}
// expected-error@-1 {{cannot convert value of type '[C : C]' to '[D : D]' in assignment, arguments to generic parameter 'Value' ('C' and 'D') are expected to be equal}}

