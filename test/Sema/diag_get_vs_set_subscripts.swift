// RUN: %target-typecheck-verify-swift

enum Key: Int {
  case aKey
  case anotherKey // expected-note {{'anotherKey' declared here}}
}
 
class sr6175 {
  var dict: [Key: String] = [:]
  func what() -> Void {
    dict[.notAKey] = "something" // expected-error {{type 'Key' has no member 'notAKey'; did you mean 'anotherKey'?}}
  }
  
  subscript(i: Int) -> Int {
    return i*i
  }
  subscript(j: Double) -> Double {
    get { return j*j }
    set {}
  }
}

let s = sr6175()
let one: Int = 1
// Should choose the settable subscript to find a problem with, not the get-only subscript
s[one] = 2.5 // expected-error {{cannot convert value of type 'Int' to expected argument type 'Double'}}
