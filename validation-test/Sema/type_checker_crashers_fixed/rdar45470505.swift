// RUN: %target-typecheck-verify-swift

extension BinaryInteger {
  init(bytes: [UInt8]) { fatalError() }

  init<S: Sequence>(bytes: S) where S.Iterator.Element == UInt8 {
    // expected-note@-1 {{incorrect labels for candidate (have: '(_:)', expected: '(bytes:)')}}
    self.init(bytes // expected-error {{no exact matches in call to initializer}}
    // expected-note@-1 {{}}

extension
// expected-error@-1 {{declaration is only valid at file scope}}
// expected-error@-2 {{expected ')' in expression list}}
// expected-error@-3 {{expected '{' in extension}}
  }
// expected-error@-1 {{expected type name in extension declaration}}
}
