// RUN: %target-typecheck-verify-swift

protocol P1 {
  subscript (i: Int) -> Int { get } // expected-note{{protocol requires subscript with type '(Int) -> Int'}}
}

class C1 : P1 {
  subscript (i: Int) -> Int {
    get {
      return i
    }
    set {}
  }
}

struct S1 : P1 {
  subscript (i: Int) -> Int {
    get {
      return i
    }
    set {}
  }
}

struct S1Error : P1 { // expected-error{{type 'S1Error' does not conform to protocol 'P1'}}
  subscript (i: Int) -> Double { // expected-note{{candidate has non-matching type '(Int) -> Double'}}
    get {
      return Double(i)
    }
    set {}
  }
}


//===----------------------------------------------------------------------===//
// Get-only property requirements
//===----------------------------------------------------------------------===//

protocol PropertyGet {
  var x : Int { get }   // expected-note {{protocol requires property 'x' with type 'Int'}}
}
  
class PropertyGet_Stored : PropertyGet {
  var x : Int = 0  // ok
}

class PropertyGet_Immutable : PropertyGet {
  let x : Int = 0 // ok.
}

class PropertyGet_ComputedGetSet : PropertyGet {
  var x : Int { get { return 0 } set {} }  // ok
}

class PropertyGet_ComputedGet : PropertyGet {
  var x : Int { return 0 }  // ok
}

struct PropertyGet_StaticVar : PropertyGet {  // expected-error {{type 'PropertyGet_StaticVar' does not conform to protocol 'PropertyGet'}}
  static var x : Int = 42  // expected-note {{candidate operates on a type, not an instance as required}}
}

//===----------------------------------------------------------------------===//
// Get-Set property requirements
//===----------------------------------------------------------------------===//

protocol PropertyGetSet {
  var x : Int { get set }  // expected-note 2{{protocol requires property 'x' with type 'Int'}}
}
  
class PropertyGetSet_Stored : PropertyGetSet {
  var x : Int = 0  // ok
}

class PropertyGetSet_Immutable : PropertyGetSet {  // expected-error {{type 'PropertyGetSet_Immutable' does not conform to protocol 'PropertyGetSet'}}
  let x : Int = 0  // expected-note {{candidate is not settable, but protocol requires it}}
}

class PropertyGetSet_ComputedGetSet : PropertyGetSet {
  var x : Int { get { return 42 } set {} }  // ok
}

class PropertyGetSet_ComputedGet : PropertyGetSet {  // expected-error {{type 'PropertyGetSet_ComputedGet' does not conform to protocol 'PropertyGetSet'}}
  var x : Int { return 42 }  // expected-note {{candidate is not settable, but protocol requires it}}
}

//===----------------------------------------------------------------------===//
// Get-only subscript requirements
//===----------------------------------------------------------------------===//


protocol SubscriptGet {
  subscript(a : Int) -> Int { get }
}

class SubscriptGet_Get : SubscriptGet {
  subscript(a : Int) -> Int { return 0 }  // ok
}

class SubscriptGet_GetSet : SubscriptGet {
  subscript(a : Int) -> Int { get { return 42 } set {} }  // ok
}


//===----------------------------------------------------------------------===//
// Get-set subscript requirements
//===----------------------------------------------------------------------===//


protocol SubscriptGetSet {
  subscript(a : Int) -> Int { get set }   // expected-note {{protocol requires subscript with type '(Int) -> Int'}}
}

class SubscriptGetSet_Get : SubscriptGetSet {  // expected-error {{type 'SubscriptGetSet_Get' does not conform to protocol 'SubscriptGetSet'}}
  subscript(a : Int) -> Int { return 0 }   // expected-note {{candidate is not settable, but protocol requires it}}
}

class SubscriptGetSet_GetSet : SubscriptGetSet {
  subscript(a : Int) -> Int { get { return 42 } set {} }  // ok
}

