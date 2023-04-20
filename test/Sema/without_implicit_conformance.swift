// RUN: %target-typecheck-verify-swift

protocol UserDefinedProtocol {}

struct NotCopyable: ~Copyable {}

class Who: ~SomebodyNew {} // expected-error {{cannot find type 'SomebodyNew' in scope}}

class Blah: ~UserDefinedProtocol {} // expected-error {{cannot suppress conformance to 'UserDefinedProtocol' on class}}

protocol Brotocol<T>: ~Copyable { // expected-error {{cannot suppress conformance to 'Copyable' on protocol}}
  associatedtype T: ~Copyable // expected-error {{cannot suppress conformance to 'Copyable' on associated type}}
  associatedtype V: ~Any // expected-error {{cannot suppress conformance to 'Any' on associated type}}
  associatedtype R: ~Self // expected-error {{suppression of non-protocol type 'Self'}}
}

func inspectGPTCoin<T: ~Copyable>(_ t: T) {}
// expected-error@-1 {{cannot suppress conformance to 'Copyable' on generic parameter}}

func weirdo<T: ~Int>(_ t: T) {}
// expected-error@-1 {{suppression of non-protocol type 'Int'}}

func checkWhere1<T>(_ t: T)
    where T: ~Copyable, // expected-error {{suppression of 'Copyable' conformance cannot appear here}}
          T: ~Hashable  // expected-error {{suppression of 'Hashable' conformance cannot appear here}}
{}

func checkWhere2<T>(_ t: T)
    where T == ~Copyable {} // expected-error {{suppression cannot appear here}}

func checkWhere3<T>(_ t: T)
    where ~Equatable == T {} // expected-error {{suppression cannot appear here}}

struct DoubleNotCopyable: ~Copyable, // expected-note {{'Copyable' previously suppressed here}}
                          Sendable,
                          ~Copyable { } // expected-error {{implicit conformance to 'Copyable' already suppressed}}

extension DoubleNotCopyable: ~Copyable {} // expected-error {{cannot suppress conformance to 'Copyable' on extension}}

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
                  where T: ~Copyable {} // expected-error {{suppression of 'Copyable' conformance cannot appear here}}

struct SuppressANonProtocolType: ~Colors {} // expected-error {{suppression of non-protocol type 'Colors'}}

struct MoreSuppressed: ~Copyable,
~Hashable, // expected-error {{cannot suppress implicit conformance to 'Hashable'}}
~Sendable, // expected-error {{cannot suppress implicit conformance to 'Sendable'}}
~Int {} // expected-error {{suppression of non-protocol type 'Int'}}


func checkIt(_ t: ~Copyable) // expected-error {{suppression cannot appear here}}
    -> ~Sendable { // expected-error {{suppression cannot appear here}}
  let y: ~Copyable = t // expected-error {{suppression cannot appear here}}
  return y
}

func bogus(_ f: ~(Int) -> Bool) { // expected-error {{suppression cannot appear here}}
  if 0 as ~Copyable {} // expected-error {{suppression cannot appear here}}

  if NotCopyable is ~Copyable {} // expected-error {{suppression cannot appear here}}

  // FIXME: kind of a bad set of errors
  let one = ~Copyable.self // expected-error {{unary operator '~' cannot be applied to an operand of type '_.Type'}}
                           // expected-error@-1 {{'Copyable' can only be suppressed via '~Copyable' at this time}}
  let two = ~Copyable.type // expected-error {{'Copyable' can only be suppressed via '~Copyable' at this time}}
}

func bogus2<T>(_ f: T) where T == ~(Int) throws -> Bool {} // expected-error {{suppression cannot appear here}}

func bogus2<T>(_ f: T) where T: ~(Copyable, Bool) {} // expected-error {{suppression cannot appear here}}

struct Bogus: ~(Int, Int) async -> Void {} // expected-error {{suppression of non-protocol type '(Int, Int) async -> Void'}}

// FIXME: poor error messages
extension ~Copyable {} // expected-error {{suppression of 'Copyable' conformance cannot appear here}}
extension ~(Int) async -> Void {} // expected-error {{suppression cannot appear here}}

// this is nicer:
extension (Int) -> Void {} // expected-error {{non-nominal type '(Int) -> Void' cannot be extended}}

func apply(_ f: (~String) -> Void) {} // expected-error {{suppression cannot appear here}}

func apply(_ f: (~Copyable...) // expected-error {{suppression cannot appear here}}
                    ->
                  ~Void) {} // expected-error {{suppression cannot appear here}}

extension Array where Element: ~Copyable {} // expected-error {{suppression of 'Copyable' conformance cannot appear here}}

extension Optional where Self: ~Sendable {} // expected-error {{suppression of 'Sendable' conformance cannot appear here}}

enum NoncopyableOptional<Element> : ~Copyable
  where Element: ~Copyable {} // expected-error {{suppression of 'Copyable' conformance cannot appear here}}
