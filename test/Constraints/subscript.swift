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
  subscript(key: String) -> Int { get {} }
  subscript(key: String) -> Double {  get {} }
}

let _ = 1["1"]  // expected-error {{cannot subscript a value of type 'Int' with an index of type 'String'}}


