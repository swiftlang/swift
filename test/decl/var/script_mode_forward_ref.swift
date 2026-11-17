// RUN: %target-typecheck-verify-swift

_ = forwardRef1 // expected-error {{use of global variable 'forwardRef1' before its declaration}}
_ = forwardRef2 // expected-error {{use of global variable 'forwardRef2' before its declaration}}
_ = forwardRef3 // expected-error {{use of global variable 'forwardRef3' before its declaration}}
_ = forwardRef4 // expected-error {{use of global variable 'forwardRef4' before its declaration}}
_ = forwardRef5 // expected-error {{use of global variable 'forwardRef5' before its declaration}}

// MARK: Closures

// For now these are valid.
let _ = {
  _ = forwardRef1
  _ = forwardRef2
  _ = forwardRef3
}
_ = {
  _ = forwardRef4
  _ = forwardRef5
}

// MARK: Functions

func forwardFn() {
  _ = forwardRef1
  _ = forwardRef2
  _ = forwardRef3
  _ = forwardRef4
  _ = forwardRef5
}

struct InAType {
  func forwardMethod() {
    _ = forwardRef1
    _ = forwardRef2
    _ = forwardRef3
    _ = forwardRef4
    _ = forwardRef5
  }
}

// MARK: Weird intialization

_ = delayedInitForwardRef1 // expected-error {{use of global variable 'delayedInitForwardRef1' before its declaration}}
let delayedInitForwardRef1: Int // expected-note {{'delayedInitForwardRef1' declared here}}
_ = delayedInitForwardRef1
delayedInitForwardRef1 = 1

_ = delayedInitForwardRef2 // expected-error {{use of global variable 'delayedInitForwardRef2' before its declaration}}
var delayedInitForwardRef2: Int // expected-note {{'delayedInitForwardRef2' declared here}}
_ = delayedInitForwardRef2
delayedInitForwardRef2 = 1

initBeforeDecl1 = 1 // expected-error {{use of global variable 'initBeforeDecl1' before its declaration}}
print(initBeforeDecl1) // expected-error {{use of global variable 'initBeforeDecl1' before its declaration}}
let initBeforeDecl1: Int // expected-note 2{{'initBeforeDecl1' declared here}}
print(initBeforeDecl1)

multiInitLet = 1 // expected-error {{use of global variable 'multiInitLet' before its declaration}}
multiInitLet = 2 // expected-error {{use of global variable 'multiInitLet' before its declaration}}
let multiInitLet: Int // expected-note 2{{'multiInitLet' declared here}}
multiInitLet = 3

// MARK: Self references

var self1 = self1 // expected-note {{'self1' declared here}}
// expected-error@-1 {{use of global variable 'self1' before its declaration}}

var self2 : Int = self2 // expected-note {{'self2' declared here}}
// expected-error@-1 {{use of global variable 'self2' before its declaration}}
var (self3) : Int = self3 // expected-note {{'self3' declared here}}
// expected-error@-1 {{use of global variable 'self3' before its declaration}}
var (self4) : Int = self4 // expected-note {{'self4' declared here}}
// expected-error@-1 {{use of global variable 'self4' before its declaration}}

var self5 = self5 + self5 // expected-note 2{{'self5' declared here}}
// expected-error@-1 2{{use of global variable 'self5' before its declaration}}

var self6 = !self6 // expected-note {{'self6' declared here}}
// expected-error@-1 {{use of global variable 'self6' before its declaration}}

var (self7a, self7b) = (self7b, self7a) // expected-note {{'self7a' declared here}} expected-note {{'self7b' declared here}}
// expected-error@-1 {{use of global variable 'self7a' before its declaration}}
// expected-error@-2 {{use of global variable 'self7b' before its declaration}}

// FIXME: These should be invalid.
var selfClosure1 : Int = { selfClosure1 }()
var (selfClosure2) : Int = { selfClosure2 }()

// MARK: Computed vars

// Forward references of computed vars in script mode is fine.
_ = forwardRefComputedVar1
var forwardRefComputedVar1: Int { 0 }

_ = forwardRefComputedVar2
var forwardRefComputedVar2: Int { get { 0 } set {} }

var forwardRefComputedVar3: Int { forwardRefComputedVar3 }
// expected-warning@-1 {{attempting to access 'forwardRefComputedVar3' within its own getter}}

// MARK: Misc

// Fine.
let refWithinSinglePBD1 = 0, refWithinSinglePBD2 = refWithinSinglePBD1

// Not fine.
let multiPBDForwardRef1 = multiPBDForwardRef2 ; let multiPBDForwardRef2 = multiPBDForwardRef1
// expected-error@-1:27 {{use of global variable 'multiPBDForwardRef2' before its declaration}}
// expected-note@-2:53 {{'multiPBDForwardRef2' declared here}}

// MARK: The declarations

let forwardRef1 = 0 // expected-note {{'forwardRef1' declared here}}
var forwardRef2 = 0 // expected-note {{'forwardRef2' declared here}}
var forwardRef3 = 0 { didSet {} } // expected-note {{'forwardRef3' declared here}}
let forwardRef4: Int // expected-note {{'forwardRef4' declared here}}
var forwardRef5: Int // expected-note {{'forwardRef5' declared here}}
