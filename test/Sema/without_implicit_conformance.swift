// RUN: %target-typecheck-verify-swift

protocol UserDefinedProtocol {}

class Who: ~SomebodyNew {} // expected-error {{cannot find type 'SomebodyNew' in scope}}

class Blah: ~UserDefinedProtocol {} // expected-error {{cannot suppress conformance on class}}

protocol Brotocol<T>: ~Copyable { // expected-error {{cannot suppress conformances here}}
  associatedtype T: ~Copyable // expected-error {{cannot suppress conformances here}}
}

func inspectGPTCoin<T: ~Copyable>(_ t: T) {}
// expected-error@-1 {{expected a class type or protocol-constrained type restricting 'T'}}

// FIXME: not a pleasant set of errors!
func piano<T>(_ t: T) where T: ~Copyable {}
// expected-error@-1 {{expected '{' in body of function declaration}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
// expected-error@-3 {{expected type}}
// expected-error@-4 {{'any Copyable' cannot be constructed because it has no accessible initializers}}

struct DoubleNotCopyable: ~Copyable, // expected-note {{'Copyable' previously suppressed here}}
                          Sendable,
                          ~Copyable { } // expected-error {{implicit conformance to 'Copyable' already suppressed}}

extension DoubleNotCopyable: ~Copyable {} // expected-error {{cannot suppress conformances here}}

enum Colors: ~Equatable, ~Hashable {
// expected-error@-1 {{cannot suppress implicit conformance to 'Equatable'}}
// expected-error@-2 {{cannot suppress implicit conformance to 'Hashable'}}
  case red
  case white
  case blue
}

// FIXME: this is confusing! We need to require parens!!
struct Composition: ~Copyable & Equatable {} // expected-error {{cannot suppress implicit conformance to 'Copyable & Equatable'}}

enum Whatever<T> : ~Copyable
                  where T: ~Copyable {} // expected-error {{expected type}}
                                        // expected-error@-1 {{expected '{' in enum}}

struct SuppressANonProtocolType: ~Colors {} // expected-error {{suppression of non-protocol type 'Colors'}}
