// RUN: %target-typecheck-verify-swift

// Simple subscript of arrays:
func simpleSubscript(_ array: [Float], x: Int) -> Float {
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

func archetypeSubscript<T : IntToStringSubscript, U : LameDictionary>(_ t: T, u: U)
       -> String {
  // Subscript an archetype.
  if false { return t[17] }

  // Subscript an archetype for which the subscript operator is in a base class.
  return u[17]
}

// Subscript of existential type.
func existentialSubscript(_ a: IntToStringSubscript) -> String {
  return a[17]
}

class MyDictionary<Key, Value> {
  subscript (key : Key) -> Value {
    get {}
  }
}

class MyStringToInt<T> : MyDictionary<String, Int> { }

// Subscript of generic type.
func genericSubscript<T>(_ t: T,
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

let squares = [ 1, 2, 3 ].reduce([:]) { (dict, n) in
  var dict = dict
  dict[n] = n * n
  return dict
}

// <rdar://problem/23670252> QoI: Misleading error message when assigning a value from [String : AnyObject]
func r23670252(_ dictionary: [String : AnyObject], someObject: AnyObject) {
  let color : String?
  color = dictionary["color"]  // expected-error {{cannot assign value of type 'AnyObject?' to type 'String?'}}
  _ = color
}


// SR-718 - Type mismatch reported as extraneous parameter
struct SR718 {
  subscript(b : Int) -> Int
    { return 0 }
  subscript(a a : UInt) -> Int { return 0 }
}

SR718()[a: Int()] // expected-error {{cannot convert value of type 'Int' to expected argument type 'UInt'}}

// rdar://problem/25601561 - Qol: Bad diagnostic for failed assignment from Any to more specific type

struct S_r25601561 {
  func value() -> Any? { return "hi" }
}

class C_r25601561 {
  var a: [S_r25601561?] = []
  func test(i: Int) -> String {
    let s: String = a[i]!.value()! // expected-error {{cannot convert value of type 'Any' to specified type 'String'}}
    return s
  }
}
