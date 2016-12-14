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

// Test dictionary forced downcasts
var dictCC = Dictionary<C, C>()
var dictCD = dictCC as! Dictionary<C, D>
var dictDC = dictCC as! Dictionary<D, C>
var dictDD = dictCC as! Dictionary<D, D>

// Test dictionary conditional downcasts
if let dictCD = dictCC as? Dictionary<C, D> { }
if let dictDC = dictCC as? Dictionary<D, C> { }
if let dictDD = dictCC as? Dictionary<D, D> { }

// Test dictionary downcasts to unrelated types.
dictCC as Dictionary<D, U> // expected-error{{cannot convert value of type 'Dictionary<C, C>' to type 'Dictionary<D, U>' in coercion}}
dictCC as Dictionary<U, D> // expected-error{{cannot convert value of type 'Dictionary<C, C>' to type 'Dictionary<U, D>' in coercion}}
dictCC as Dictionary<U, U> // expected-error{{cannot convert value of type 'Dictionary<C, C>' to type 'Dictionary<U, U>' in coercion}}

// Test dictionary conditional downcasts to unrelated types
if let dictDU = dictCC as? Dictionary<D, U> { } // expected-error{{'Dictionary<C, C>' is not convertible to 'Dictionary<D, U>'}}
if let dictUD = dictCC as? Dictionary<U, D> { } // expected-error{{'Dictionary<C, C>' is not convertible to 'Dictionary<U, D>'}}
if let dictUU = dictCC as? Dictionary<U, U> { } // expected-error{{'Dictionary<C, C>' is not convertible to 'Dictionary<U, U>'}}

