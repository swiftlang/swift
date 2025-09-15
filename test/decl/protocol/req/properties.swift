// RUN: %target-typecheck-verify-swift

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

struct PropertyGet_StaticVar : PropertyGet {  // expected-error {{type 'PropertyGet_StaticVar' does not conform to protocol 'PropertyGet'}} expected-note {{add stubs for conformance}}
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

class PropertyGetSet_Immutable : PropertyGetSet {  // expected-error {{type 'PropertyGetSet_Immutable' does not conform to protocol 'PropertyGetSet'}} expected-note {{add stubs for conformance}}
  let x : Int = 0  // expected-note {{candidate is not settable, but protocol requires it}}
}

class PropertyGetSet_ComputedGetSet : PropertyGetSet {
  var x : Int { get { return 42 } set {} }  // ok
}

class PropertyGetSet_ComputedGet : PropertyGetSet {  // expected-error {{type 'PropertyGetSet_ComputedGet' does not conform to protocol 'PropertyGetSet'}} expected-note {{add stubs for conformance}}
  var x : Int { return 42 }  // expected-note {{candidate is not settable, but protocol requires it}}
}
