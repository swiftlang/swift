// RUN: %target-typecheck-verify-swift



protocol Sando { func make() } // expected-note {{protocol requires function 'make()'}}
                               // expected-note@-1 {{type 'U' does not conform to inherited protocol 'Copyable'}}

struct BuggerView: ~Escapable {} // expected-error {{can only suppress 'Copyable'}}

struct S: ~U, // expected-error {{can only suppress 'Copyable'}}
              // expected-error@-1 {{inheritance from non-protocol type 'U'}}
          ~Copyable {}

struct U:  // expected-error {{type 'U' does not conform to protocol 'Copyable'}}
          ~Copyable,
          Sando,
          ~Copyable // expected-error {{duplicate suppression of 'Copyable'}}
          {}


// The expected behavior for '~' in the inheritance clause of a decl not supporting
// suppression is to emit an error and then to treat it as if it's inheriting from
// the type, rather than suppressing. That is, it treats it like the '~' wasn't there
// after emitting an error.

class C: // expected-error {{type 'C' does not conform to protocol 'Sando'}}
         ~Copyable, // expected-error {{cannot suppress conformances here}}
         ~Sando // expected-error {{cannot suppress conformances here}}
         {}

protocol Rope<Element>: ~Copyable { // expected-error {{cannot suppress conformances here}}

  associatedtype Element: ~Copyable // expected-error {{cannot suppress conformances here}}
}

extension S: ~Copyable {} // expected-error {{cannot suppress conformances here}}
                          // expected-error@-1 {{struct 'S' required to be 'Copyable' but is marked with '~Copyable'}}

func takeNoncopyableGeneric<T: ~Copyable>(_ t: T) {} // expected-error {{cannot suppress conformances here}}

@_moveOnly struct ExtraNonCopyable:         // expected-error {{duplicate attribute}}{{1-12=}}
                                  ~Copyable // expected-note {{attribute already specified here}}
                                  {}

// basic tests to ensure it's viewed as a noncopyable struct, by using 
// capabilities only available to them
struct HasADeinit: ~Copyable { deinit {} }

public struct MoveOnlyS1<T> : ~Copyable { deinit {} }
public struct MoveOnlyS2<T: Equatable> : ~Copyable { deinit {} }
public struct MoveOnlyS3<T: ~Copyable> : ~Copyable { deinit {} } // expected-error {{cannot suppress conformances here}}

public enum MoveOnlyE1<T> : ~Copyable { 
  case holding(s: MoveOnlyS1<T>)
  consuming func x() {}
}

public enum MoveOnlyE2<T: Equatable> : ~Copyable { 
  case holding(s: MoveOnlyS1<T>)
  consuming func x() {}
}

func more() {
  let _: any ~Copyable = 19  // expected-error@:14 {{cannot suppress conformances here}}

  let _: any ~Equatable = 19  // expected-error@:14 {{cannot suppress conformances here}}
}

func blah<T>(_ t: T) where T: ~Copyable,    // expected-error@:31 {{cannot suppress conformances here}}

                           T: ~Hashable {}  // expected-error@:31 {{cannot suppress conformances here}}

func foo<T: ~Copyable>(x: T) {} // expected-error {{cannot suppress conformances here}}

struct Buurap<T: ~Copyable> {} // expected-error {{cannot suppress conformances here}}

protocol Foo where Self: ~Copyable {} // expected-error {{cannot suppress conformances here}}
