// RUN: %target-typecheck-verify-swift -target %target-future-triple -enable-experimental-feature NoncopyableCasting
// RUN: %if OS=macosx %{ %target-typecheck-verify-swift -target %target-cpu-apple-macosx13.0 -enable-experimental-feature NoncopyableCasting -verify-additional-prefix older- %}

// REQUIRES: swift_feature_NoncopyableCasting

// A cast between a fully concrete non-`Copyable` type and a type that can only
// hold `Copyable` values can never succeed, whatever the dynamic type. It is
// diagnosed as always failing, and, since it never copies anything, needs no
// newer runtime. Casts whose answer depends on a dynamic type -- an
// existential, a generic parameter, or an Optional -- are runtime questions:
// no warning, and at an older deployment target an availability error.

protocol P: ~Copyable {}
protocol CP {}
protocol Q: ~Copyable {}
struct MO: P, ~Copyable {}
struct NCBox<T>: ~Copyable { var t: T }

func concreteToCopyable(_ mo: borrowing MO, _ b: borrowing NCBox<Int>) {
  _ = mo is AnyHashable // expected-warning {{cast from 'MO' to unrelated type 'AnyHashable' always fails}}
  _ = mo is any CP // expected-warning {{cast from 'MO' to unrelated type 'any CP' always fails}}
  _ = mo is Any // expected-warning {{cast from 'MO' to unrelated type 'Any' always fails}}
  _ = mo is Int // expected-warning {{cast from 'MO' to unrelated type 'Int' always fails}}
  _ = b is Int // expected-warning {{cast from 'NCBox<Int>' to unrelated type 'Int' always fails}}
}

func copyableToConcrete(_ a: Any, _ cp: any CP) {
  _ = a is MO // expected-warning {{cast from 'Any' to unrelated type 'MO' always fails}}
  _ = cp is MO // expected-warning {{cast from 'any CP' to unrelated type 'MO' always fails}}
  _ = a as MO // expected-error {{cannot convert value of type 'Any' to type 'MO' in coercion}}
}

func runtimeQuestions(_ e: borrowing any P & ~Copyable, _ mo: borrowing MO,
                      _ cp: any CP) -> Bool {
  // expected-older-note @-2 2 {{add '@available' attribute to enclosing global function}}
  let r1 = e is MO
  // expected-older-error @-1 {{runtime support for casting out of a non-'Copyable' value is only available in}}
  // expected-older-note @-2 {{add 'if #available' version check}}
  let r2 = mo is any Q & ~Copyable
  // expected-older-error @-1 {{runtime support for casting out of a non-'Copyable' value is only available in}}
  // expected-older-note @-2 {{add 'if #available' version check}}
  let r3 = cp is any Q & ~Copyable // a Copyable source: no newer runtime needed
  return r1 || r2 || r3
}
