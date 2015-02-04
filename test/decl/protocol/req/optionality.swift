// RUN: %target-parse-verify-swift

@objc class C1 { }
@objc class C2 { }

// ------------------------------------------------------------------------
// Parameters of IUO type.
// ------------------------------------------------------------------------
@objc protocol ParameterIUO1 {
  optional func f0(x: C1!)
}

@objc class ParameterIUO1a : ParameterIUO1 {
  func f0(x: C1!) { } // okay: exact match
}

@objc class ParameterIUO1b : ParameterIUO1 {
  func f0(x: C1) { } // okay: all is permitted with IUO requirements
}

@objc class ParameterIUO1c : ParameterIUO1 {
  func f0(x: C1?) { } // okay: all is permitted with IUO requirements
}

// ------------------------------------------------------------------------
// Parameters of optional type.
// ------------------------------------------------------------------------

@objc protocol ParameterOpt1 {
  optional func f0(x: C1?) // expected-note 2{{declared here}}
}

@objc class ParameterOpt1a : ParameterOpt1 {
  func f0(x: C1?) { } // okay: exact match
}

@objc class ParameterOpt1b : ParameterOpt1 {
  func f0(x: C1!) { } // expected-warning{{different optionality than expected}}{{16-17=?}}
}

@objc class ParameterOpt1c : ParameterOpt1 {
  func f0(x: C1) { } // expected-error{{different optionality than required}}{{16-16=?}}
}

// ------------------------------------------------------------------------
// Parameters of non-optional type.
// ------------------------------------------------------------------------
@objc protocol ParameterNonOpt1 {
  optional func f0(x: C1) // expected-note 2{{declared here}}
}

@objc class ParameterNonOpt1a : ParameterNonOpt1 {
  func f0(x: C1) { } // okay: exact match
}

@objc class ParameterNonOpt1b : ParameterNonOpt1 {
  func f0(x: C1!) { } // expected-warning{{parameter of 'f0' has different optionality than expected by protocol 'ParameterNonOpt1'}}{{16-17=}}
}

@objc class ParameterNonOpt1c : ParameterNonOpt1 {
  func f0(x: C1?) { } // expected-warning{{parameter of 'f0' has different optionality than expected by protocol 'ParameterNonOpt1'}}{{16-17=}}
}

// ------------------------------------------------------------------------
// Result of IUO type.
// ------------------------------------------------------------------------
@objc protocol ResultIUO1 {
  optional func f0() -> C1!
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

// ------------------------------------------------------------------------
// Result of optional type.
// ------------------------------------------------------------------------
@objc protocol ResultOpt1 {
  optional func f0() -> C1? // expected-note 2{{declared here}}
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

// ------------------------------------------------------------------------
// Result of non-optional type.
// ------------------------------------------------------------------------
@objc protocol ResultNonOpt1 {
  optional func f0() -> C1 // expected-note 2 {{declared here}}
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

// ------------------------------------------------------------------------
// Multiple parameter mismatches
// ------------------------------------------------------------------------
@objc protocol MultiParamsOpt1 {
  optional func f0(x: C1?, y: C1) // expected-note{{here}}
}

@objc class MultiParamsOpt1a : MultiParamsOpt1 {
  func f0(x: C1!, y: C1!) { } // expected-warning{{parameters of 'f0(_:y:)' have different optionality than expected}}{{16-17=?}}{{24-25=}}
}

// ------------------------------------------------------------------------
// Parameter and result type mismatches
// ------------------------------------------------------------------------
@objc protocol ParamAndResult1 {
  optional func f0(x: C1?) -> C1 // expected-note{{here}}
}

@objc class ParamAndResult1a : ParamAndResult1 {
  func f0(x: C1!) -> C1! { } // expected-warning{{result and parameters of 'f0' have different optionality than expected}}{{16-17=?}}{{24-25=}}
}
