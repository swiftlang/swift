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

//===----------------------------------------------------------------------===//
// Generic subscript requirements
//===----------------------------------------------------------------------===//

protocol Initable {
  init()
}

protocol GenericSubscriptProtocol {
  subscript<T : Initable>(t: T.Type) -> T { get set }
  // expected-note@-1 {{protocol requires subscript with type '<T where T : Initable> (T.Type) -> T'; do you want to add a stub?}}
}

struct GenericSubscriptWitness : GenericSubscriptProtocol {
  subscript<T : Initable>(t: T.Type) -> T {
    get {
      return t.init()
    }

    set { }
  }
}

struct GenericSubscriptNoWitness : GenericSubscriptProtocol {}
// expected-error@-1 {{type 'GenericSubscriptNoWitness' does not conform to protocol 'GenericSubscriptProtocol'}}
