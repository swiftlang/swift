// RUN: %target-typecheck-verify-swift

extension BinaryInteger {
  init(bytes: [UInt8]) { fatalError() }

  init<S: Sequence>(bytes: S) where S.Iterator.Element == UInt8 {
    self.init(bytes // expected-error {{ambiguous reference to initializer 'init(_:)'}}
// expected-note@-1 {{}}

extension
// expected-error@-1 {{declaration is only valid at file scope}}
// expected-error@-2 {{expected ')' in expression list}}
// expected-error@-3 {{expected '{' in extension}}
  }
// expected-error@-1 {{expected type name in extension declaration}}
}
