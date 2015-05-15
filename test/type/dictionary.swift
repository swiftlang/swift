// RUN: %target-parse-verify-swift

// Dictionary types.
class Base {
  func f0(d: [String: Int]) { }
  func f1(d: [String: [Int: Int]]) { }
}

class Derived : Base {
  override func f0(d: Dictionary<String, Int>) { }
  override func f1(d: Dictionary<String, Dictionary<Int, Int>>) { }
}

// Dictionary types in generic specializations.
struct X<T> { }

func testGenericSpec() {
  _ = X<[Int : Int]>()
}

// Dictionary types for construction.
func constructDictionary(n: Int) {
  var dict = [Int : String](minimumCapacity: n)
  dict[5] = "hello"
}

// Parse errors
var y1: [String : Int = ["hello" : 1] // expected-error{{expected ']' in dictionary type}}
  // expected-note @-1{{to match this opening '['}}
var y2: [String : ] // expected-error{{expected dictionary value type}}


struct NotHashable { }

var nh1 : [NotHashable : Int ] // expected-error{{'NotHashable' does not conform to protocol 'Hashable'}}
