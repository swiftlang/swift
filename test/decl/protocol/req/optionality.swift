// RUN: %target-typecheck-verify-swift -enable-objc-interop

@objc class C1 { }
@objc class C2 { }

// ------------------------------------------------------------------------
// Parameters of IUO type.
// ------------------------------------------------------------------------
@objc protocol ParameterIUO1 {
  @objc optional func f0(_ x: C1!)
}

@objc class ParameterIUO1a : ParameterIUO1 {
  func f0(_ x: C1!) { } // okay: exact match
}

@objc class ParameterIUO1b : ParameterIUO1 {
  func f0(_ x: C1) { } // okay: all is permitted with IUO requirements
}

@objc class ParameterIUO1c : ParameterIUO1 {
  func f0(_ x: C1?) { } // okay: all is permitted with IUO requirements
}

// A non-@objc protocol requires an exact match instead.
protocol ParameterIUO2 {
  func f0(_ x: C1!) // expected-note{{protocol requires function 'f0' with type '(C1?) -> ()'}}
}

struct ParameterIUO2a : ParameterIUO2 {
  func f0(_ x: C1!) { } // okay: exact match
}

struct ParameterIUO2b : ParameterIUO2 {
  // expected-error@-1{{type 'ParameterIUO2b' does not conform to protocol 'ParameterIUO2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0(_ x: C1?) { } // expected-note{{candidate parameter type is an optional, but the protocol requires an implicitly unwrapped optional}}{{18-19=!}}
}

// ------------------------------------------------------------------------
// Parameters of optional type.
// ------------------------------------------------------------------------

@objc protocol ParameterOpt1 {
  @objc optional func f0(_ x: C1?) // expected-note 2{{declared here}}
}

@objc class ParameterOpt1a : ParameterOpt1 {
  func f0(_ x: C1?) { } // okay: exact match
}

@objc class ParameterOpt1b : ParameterOpt1 {
  func f0(_ x: C1!) { } // expected-warning{{different optionality than expected}}{{18-19=?}}
}

@objc class ParameterOpt1c : ParameterOpt1 {
  func f0(_ x: C1) { } // expected-error{{different optionality than required}}{{18-18=?}}
}

// A non-@objc protocol rejects the implicitly unwrapped witness. Both types
// print as 'C1?', so the candidate note names the difference.
protocol ParameterOpt2 {
  func f0(_ x: C1?) // expected-note{{protocol requires function 'f0' with type '(C1?) -> ()'}}
}

struct ParameterOpt2a : ParameterOpt2 {
  func f0(_ x: C1?) { } // okay: exact match
}

struct ParameterOpt2b : ParameterOpt2 {
  // expected-error@-1{{type 'ParameterOpt2b' does not conform to protocol 'ParameterOpt2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0(_ x: C1!) { } // expected-note{{candidate parameter type is an implicitly unwrapped optional, but the protocol requires an optional}}{{18-19=?}}
}

// ------------------------------------------------------------------------
// Parameters of non-optional type.
// ------------------------------------------------------------------------
@objc protocol ParameterNonOpt1 {
  @objc optional func f0(_ x: C1) // expected-note 3 {{declared here}}
}

@objc class ParameterNonOpt1a : ParameterNonOpt1 {
  func f0(_ x: C1) { } // okay: exact match
}

@objc class ParameterNonOpt1b : ParameterNonOpt1 {
  func f0(_ x: C1!) { } // expected-warning{{parameter of 'f0' has different optionality than expected by protocol 'ParameterNonOpt1'}}{{18-19=}}
}

@objc class ParameterNonOpt1c : ParameterNonOpt1 {
  func f0(_ x: C1?) { } // expected-warning{{parameter of 'f0' has different optionality than expected by protocol 'ParameterNonOpt1'}}{{18-19=}}
}

@objc class ParameterNonOpt1d {
  func f0(_ x: C1?) { } // expected-note {{'f0' declared here}} {{none}}
}
extension ParameterNonOpt1d : ParameterNonOpt1 {} // expected-warning{{parameter of 'f0' has different optionality than expected by protocol 'ParameterNonOpt1'}} {{none}}

// A non-@objc protocol reports the type conflict, because the witness differs
// by more than the implicit unwrapping.
protocol ParameterNonOpt2 {
  func f0(_ x: C1) // expected-note{{protocol requires function 'f0' with type '(C1) -> ()'}}
}

struct ParameterNonOpt2a : ParameterNonOpt2 {
  // expected-error@-1{{type 'ParameterNonOpt2a' does not conform to protocol 'ParameterNonOpt2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0(_ x: C1!) { } // expected-note{{candidate has non-matching type '(C1?) -> ()'}}
}

// ------------------------------------------------------------------------
// Result of IUO type.
// ------------------------------------------------------------------------
@objc protocol ResultIUO1 {
  @objc optional func f0() -> C1!
}

@objc class ResultIUO1a : ResultIUO1 {
  func f0() -> C1! { return nil } // okay: exact match
}

@objc class ResultIUO1b : ResultIUO1 {
  func f0() -> C1 { } // okay: all is permitted with IUO requirements
}

@objc class ResultIUO1c : ResultIUO1 {
  func f0() -> C1? { } // okay: all is permitted with IUO requirements
}

// A non-@objc protocol requires an exact match instead, for a property as well
// as for a function result.
protocol ResultIUO2 {
  func f0() -> C1! // expected-note{{protocol requires function 'f0()' with type '() -> C1?'}}
  var v: C1! { get } // expected-note{{protocol requires property 'v' with type 'C1?'}}
}

struct ResultIUO2a : ResultIUO2 {
  func f0() -> C1! { nil } // okay: exact match
  var v: C1! // okay: exact match
}

struct ResultIUO2b : ResultIUO2 {
  // expected-error@-1{{type 'ResultIUO2b' does not conform to protocol 'ResultIUO2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0() -> C1? { nil } // expected-note{{candidate result type is an optional, but the protocol requires an implicitly unwrapped optional}}{{18-19=!}}
  var v: C1? // expected-note{{candidate type is an optional, but the protocol requires an implicitly unwrapped optional}}{{12-13=!}}
}

// ------------------------------------------------------------------------
// Result of optional type.
// ------------------------------------------------------------------------
@objc protocol ResultOpt1 {
  @objc optional func f0() -> C1? // expected-note 2{{declared here}}
}

@objc class ResultOpt1a : ResultOpt1 {
  func f0() -> C1? { return nil } // okay: exact match
}

@objc class ResultOpt1b : ResultOpt1 {
  func f0() -> C1 { } // expected-warning{{different optionality}}{{18-18=?}}
}

@objc class ResultOpt1c : ResultOpt1 {
  func f0() -> C1! { } // expected-warning{{different optionality}}{{18-19=?}}
}

// A non-@objc protocol rejects the implicitly unwrapped witness, for a property
// and a subscript element as well as for a function result.
protocol ResultOpt2 {
  func f0() -> C1? // expected-note{{protocol requires function 'f0()' with type '() -> C1?'}}
  var v: C1? { get } // expected-note{{protocol requires property 'v' with type 'C1?'}}
  subscript(i: Int) -> C1? { get } // expected-note{{protocol requires subscript with type '(Int) -> C1?'}}
}

struct ResultOpt2a : ResultOpt2 {
  func f0() -> C1? { nil } // okay: exact match
  var v: C1? // okay: exact match
  subscript(i: Int) -> C1? { nil } // okay: exact match
}

struct ResultOpt2b : ResultOpt2 {
  // expected-error@-1{{type 'ResultOpt2b' does not conform to protocol 'ResultOpt2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0() -> C1! { nil } // expected-note{{candidate result type is an implicitly unwrapped optional, but the protocol requires an optional}}{{18-19=?}}
  var v: C1! // expected-note{{candidate type is an implicitly unwrapped optional, but the protocol requires an optional}}{{12-13=?}}
  subscript(i: Int) -> C1! { nil } // expected-note{{candidate result type is an implicitly unwrapped optional, but the protocol requires an optional}}{{26-27=?}}
}

// ------------------------------------------------------------------------
// Result of non-optional type.
// ------------------------------------------------------------------------
@objc protocol ResultNonOpt1 {
  @objc optional func f0() -> C1 // expected-note 2 {{declared here}}
}

@objc class ResultNonOpt1a : ResultNonOpt1 {
  func f0() -> C1 { } // okay: exact match
}

@objc class ResultNonOpt1b : ResultNonOpt1 {
  func f0() -> C1? { } // expected-error{{different optionality than required}}{{18-19=}}
}

@objc class ResultNonOpt1c : ResultNonOpt1 {
  func f0() -> C1! { } // expected-warning{{different optionality}}{{18-19=}}
}

// A non-@objc protocol reports the type conflict, because the witness differs
// by more than the implicit unwrapping.
protocol ResultNonOpt2 {
  func f0() -> C1 // expected-note{{protocol requires function 'f0()' with type '() -> C1'}}
}

struct ResultNonOpt2a : ResultNonOpt2 {
  // expected-error@-1{{type 'ResultNonOpt2a' does not conform to protocol 'ResultNonOpt2'}}
  // expected-note@-2{{add stubs for conformance}}
  func f0() -> C1! { nil } // expected-note{{candidate has non-matching type '() -> C1?'}}
}

// ------------------------------------------------------------------------
// Multiple parameter mismatches
// ------------------------------------------------------------------------
@objc protocol MultiParamsOpt1 {
  @objc optional func f0(_ x: C1?, y: C1) // expected-note{{here}}
}

@objc class MultiParamsOpt1a : MultiParamsOpt1 {
  func f0(_ x: C1!, y: C1!) { } // expected-warning{{parameters of 'f0(_:y:)' have different optionality than expected}}{{18-19=?}}{{26-27=}}
}

// ------------------------------------------------------------------------
// Parameter and result type mismatches
// ------------------------------------------------------------------------
@objc protocol ParamAndResult1 {
  @objc optional func f0(_ x: C1?) -> C1 // expected-note{{here}}
}

@objc class ParamAndResult1a : ParamAndResult1 {
  func f0(_ x: C1!) -> C1! { } // expected-warning{{result and parameters of 'f0' have different optionality than expected}}{{18-19=?}}{{26-27=}}
}
