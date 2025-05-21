// RUN: %target-typecheck-verify-swift \
// RUN:   -disable-availability-checking

// These tests complement lifetime_depend_noattr.swift. If you add a test here, add one there.

// Check that functions that require lifetime dependence are prohibited without the flag.

// Don't allow empty initialization.
struct EmptyNonEscapable: ~Escapable {} // expected-error{{an implicit initializer cannot return a ~Escapable result}}

// Don't allow non-Escapable return values.
func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a function cannot return a ~Escapable result}}

func neInout(span: inout RawSpan) {} // OK

func neInoutNEParam(span: inout RawSpan, _: RawSpan) {} // expected-error{{a function cannot have a ~Escapable 'inout' parameter 'span'}}

struct S {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method cannot return a ~Escapable result}}

  func neInout(span: inout RawSpan) {} // OK

  func neInoutNEParam(span: inout RawSpan, _: RawSpan) {} // expected-error{{a method cannot have a ~Escapable 'inout' parameter 'span'}}

  mutating func mutatingNEInout(span: inout RawSpan) {} // OK

  mutating func mutatingNEInoutParam(span: inout RawSpan, _: RawSpan) {} // expected-error{{a mutating method cannot have a ~Escapable 'inout' parameter 'span'}}
}

class C {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method cannot return a ~Escapable result}}

  func neInout(span: inout RawSpan) {} // OK
}

extension MutableSpan {
  func method() {} // OK

  mutating func mutatingMethod() {} // expected-error{{a mutating method cannot have a ~Escapable 'self'}}

  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method cannot return a ~Escapable result}}

  func neInout(span: inout RawSpan) {} // expected-error{{a method cannot have a ~Escapable 'inout' parameter 'span'}}

  mutating func mutatingNEInout(span: inout RawSpan) {} // expected-error{{a mutating method cannot have a ~Escapable 'self'}}
  // expected-error@-1{{a mutating method cannot have a ~Escapable 'inout' parameter 'span'}}
}

extension Span {
  mutating func mutate() {} // expected-error{{a mutating method cannot have a ~Escapable 'self'}}
}
