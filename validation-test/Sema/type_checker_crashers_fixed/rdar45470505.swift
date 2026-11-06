// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

extension BinaryInteger { // expected-astgen-note {{to match this opening '{'}}
  init(bytes: [UInt8]) { fatalError() }

  init<S: Sequence>(bytes: S) where S.Iterator.Element == UInt8 { // expected-note {{incorrect labels for candidate (have: '(_:)', expected: '(bytes:)')}}
    self.init(bytes // expected-error {{no exact matches in call to initializer}} expected-astgen-error {{expected ')' to end function call}}
    // expected-note@-1 {{}}

extension
// expected-error@-1 {{declaration is only valid at file scope}}
// expected-cparser-error@-2 {{expected ')' in expression list}} expected-astgen-error@-2 {{expected type in extension}}
// expected-error@-3 {{expected '{' in extension}}
  }
// expected-cparser-error@-1 {{expected type name in extension declaration}}
} // expected-astgen-error {{expected '}' to end extension}}
