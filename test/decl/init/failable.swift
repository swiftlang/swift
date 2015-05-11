// RUN: %target-parse-verify-swift

struct S0 {
  init!(int: Int) { }
  init! (uint: UInt) { }
  init !(float: Float) { }

  init?(string: String) { }
  init ?(double: Double) { }
  init ? (char: Character) { }
}

struct S1<T> {
  init?(value: T) { }
}

class DuplicateDecls {
  init!() { } // expected-note{{'init()' previously declared here}}
  init?() { } // expected-error{{invalid redeclaration of 'init()'}}

  init!(string: String) { } // expected-note{{'init(string:)' previously declared here}}
  init(string: String) { } // expected-error{{invalid redeclaration of 'init(string:)'}}

  init(double: Double) { } // expected-note{{'init(double:)' previously declared here}}
  init?(double: Double) { } // expected-error{{invalid redeclaration of 'init(double:)'}}
}

// Construct via a failable initializer.
func testConstruction(i: Int, s: String) {
  let s0Opt = S0(string: s)
  assert(s0Opt != nil)
  var _: S0 = s0Opt // expected-error{{value of optional type 'S0?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let s0IUO = S0(int: i)
  assert(s0IUO != nil)
  
  _ = s0IUO
}

// ----------------------------------------------------------------------------
// Superclass initializer chaining
// ----------------------------------------------------------------------------
class Super {
  init?(fail: String) { }
  init!(failIUO: String) { }
  init() { } // expected-note 2{{non-failable initializer 'init()' overridden here}}
}

class Sub : Super {
  override init() { super.init() } // okay, never fails

  init(nonfail: Int) { // expected-note{{propagate the failure with 'init?'}}{{7-7=?}}
    super.init(fail: "boom") // expected-error{{a non-failable initializer cannot chain to failable initializer 'init(fail:)' written with 'init?'}}
  }

  init(nonfail2: Int) { // okay, traps on nil
    super.init(failIUO: "boom")
  }

  override init?(fail: String) {
    super.init(fail: fail) // okay, propagates ?
  }

  init?(fail2: String) { // okay, propagates ! as ?
    super.init(failIUO: fail2)
  }

  init?(fail3: String) { // okay, can introduce its own failure
    super.init()
  }

  override init!(failIUO: String) {
    super.init(failIUO: failIUO) // okay, propagates !
  }

  init!(failIUO2: String) { // okay, propagates ? as !
    super.init(fail: failIUO2)
  }

  init!(failIUO3: String) { // okay, can introduce its own failure
    super.init()
  }
}

// ----------------------------------------------------------------------------
// Initializer delegation
// ----------------------------------------------------------------------------
extension Super {
  convenience init(convenienceNonFailNonFail: String) { // okay, non-failable
    self.init()
  }

  convenience init(convenienceNonFailFail: String) { // expected-note{{propagate the failure with 'init?'}}{{19-19=?}}
    self.init(fail: convenienceNonFailFail) // expected-error{{a non-failable initializer cannot delegate to failable initializer 'init(fail:)' written with 'init?'}}
  }

  convenience init(convenienceNonFailFailIUO: String) { // okay, trap on failure
    self.init(failIUO: convenienceNonFailFailIUO) 
  }

  convenience init?(convenienceFailNonFail: String) { 
    self.init() // okay, can introduce its own failure
  }

  convenience init?(convenienceFailFail: String) {
    self.init(fail: convenienceFailFail) // okay, propagates ?
  }

  convenience init?(convenienceFailFailIUO: String) { // okay, propagates ! as ?
    self.init(failIUO: convenienceFailFailIUO) 
  }

  convenience init!(convenienceFailIUONonFail: String) { 
    self.init() // okay, can introduce its own failure
  }

  convenience init!(convenienceFailIUOFail: String) {
    self.init(fail: convenienceFailIUOFail) // okay, propagates ? as !
  }

  convenience init!(convenienceFailIUOFailIUO: String) { // okay, propagates !
    self.init(failIUO: convenienceFailIUOFailIUO) 
  }
}

struct SomeStruct {
   init(nonFail: Int) { // expected-note{{propagate the failure with 'init?'}}{{8-8=?}}
    self.init(fail: nonFail) // expected-error{{a non-failable initializer cannot delegate to failable initializer 'init(fail:)' written with 'init?'}}
  }

  init?(fail: Int) {}
}

// ----------------------------------------------------------------------------
// Initializer overriding
// ----------------------------------------------------------------------------
class Sub2 : Super {
  override init!(fail: String) { // okay to change ? to !
    super.init(fail: fail) 
  }
  override init?(failIUO: String) { // okay to change ! to ?
    super.init(failIUO: failIUO)
  }

  override init() { super.init() } // no change
}

// Dropping optionality
class Sub3 : Super {
  override init(fail: String) { // okay, strengthened result type
    super.init()
  }

  override init(failIUO: String) { // okay, strengthened result type
    super.init()
  }

  override init() { } // no change
}

// Adding optionality
class Sub4 : Super {
  override init?(fail: String) { super.init() }
  override init!(failIUO: String) { super.init() }

  override init?() { // expected-error{{failable initializer 'init()' cannot override a non-failable initializer}}
    super.init()
  }
}

class Sub5 : Super {
  override init?(fail: String) { super.init() }
  override init!(failIUO: String) { super.init() }

  override init!() { // expected-error{{failable initializer 'init()' cannot override a non-failable initializer}}
    super.init()
  }
}

// ----------------------------------------------------------------------------
// Initializer conformances
// ----------------------------------------------------------------------------
protocol P1 {
  init(string: String)
}

@objc protocol P1_objc {
  init(string: String)
}

protocol P2 {
  init?(fail: String)
}

protocol P3 {
  init!(failIUO: String)
}

class C1a : P1 {
  required init?(string: String) { } // expected-error{{non-failable initializer requirement 'init(string:)' cannot be satisfied by a failable initializer ('init?')}}
}

class C1b : P1 {
  required init!(string: String) { } // okay
}

class C1b_objc : P1_objc {
  @objc required init!(string: String) { } // expected-error{{non-failable initializer requirement 'init(string:)' in Objective-C protocol cannot be satisfied by a failable initializer ('init!')}}
}

class C2a : P2 {
  required init(fail: String) { } // okay to remove failability
}

class C2b : P2 {
  required init?(fail: String) { } // okay, ? matches
}

class C2c : P2 {
  required init!(fail: String) { } // okay to satisfy init? with init!
}

class C3a : P3 {
  required init(failIUO: String) { } // okay to remove failability
}

class C3b : P3 {
  required init?(failIUO: String) { } // okay to satisfy ! with ?
}

class C3c : P3 {
  required init!(failIUO: String) { } // okay, ! matches
}


// ----------------------------------------------------------------------------
// Initiating failure
// ----------------------------------------------------------------------------
struct InitiateFailureS {
  init(string: String) { // expected-note{{use 'init?' to make the initializer 'init(string:)' failable}}{{7-7=?}}
    return (nil) // expected-error{{only a failable initializer can return 'nil'}}
  }

  init(int: Int) {
    return 0 // expected-error{{'nil' is the only return value permitted in an initializer}}
  }

  init?(double: Double) {
    return nil // ok
  }

  init!(char: Character) {
    return nil // ok
  }
}
