// RUN: %target-parse-verify-swift

// Simple subscript of arrays:
func simpleSubscript(array: [Float], x: Int) -> Float {
  _ = array[x]
  return array[x]
}

// Subscript of archetype.
protocol IntToStringSubscript {
  subscript (i : Int) -> String { get }
}

class LameDictionary {
  subscript (i : Int) -> String {
    get {
      return String(i)
    }
  }
}

func archetypeSubscript<T : IntToStringSubscript, U : LameDictionary>(t: T, u: U)
       -> String {
  // Subscript an archetype.
  if false { return t[17] }

  // Subscript an archetype for which the subscript operator is in a base class.
  return u[17]
}

// Subscript of existential type.
func existentialSubscript(a: IntToStringSubscript) -> String {
  return a[17]
}

class MyDictionary<Key, Value> {
  subscript (key : Key) -> Value {
    get {}
  }
}

class MyStringToInt<T> : MyDictionary<String, Int> { }

// Subscript of generic type.
func genericSubscript<T>(t: T,
                         array: Array<Int>,
                         i2i: MyDictionary<Int, Int>,
                         t2i: MyDictionary<T, Int>,
                         s2i: MyStringToInt<()>) -> Int {
  if true { return array[5] }
  if true { return i2i[5] }
  if true { return t2i[t] }
  return s2i["hello"]
}



// <rdar://problem/21364448> QoI: Poor error message for ambiguous subscript call
extension String {
  func number() -> Int {  }     // expected-note {{found this candidate}}
  func number() -> Double {  }  // expected-note {{found this candidate}}
}

let _ = "a".number  // expected-error {{ambiguous use of 'number()'}}

extension Int {
  subscript(key: String) -> Int { get {} }      // expected-note {{found this candidate}}
  subscript(key: String) -> Double {  get {} }   // expected-note {{found this candidate}}
}

let _ = 1["1"]  // expected-error {{ambiguous use of 'subscript'}}


// rdar://17687826 - QoI: error message when reducing to an untyped dictionary isn't helpful
let squares = [ 1, 2, 3 ].reduce([:]) { (dict, n) in // expected-error {{cannot invoke 'reduce' with an argument list of type '([_ : _], @noescape (_, Int) throws -> _)'}}
  // expected-note @-1 {{expected an argument list of type '(T, combine: @noescape (T, Int) throws -> T)'}}
  var dict = dict

  dict[n] = n * n
  return dict
}

// <rdar://problem/23670252> QoI: Misleading error message when assigning a value from [String : AnyObject]
func r23670252(dictionary: [String : AnyObject], someObject: AnyObject) {
  let color : String?
  color = dictionary["color"]  // expected-error {{cannot assign value of type 'AnyObject?' to type 'String?'}}
  _ = color
}



