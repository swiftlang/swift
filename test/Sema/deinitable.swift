// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// The compiler reserves `Deinitable` for its own use.

struct S: ~Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
struct ND: ~Copyable, ~Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
enum E: ~Copyable, ~Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}

func suppressing<T: ~Copyable & ~Deinitable>(_: borrowing T) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
func requiring<T: Deinitable>(_: T) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
func whereClause<T>(_: T) where T: Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}

typealias D = Deinitable // expected-error {{'Deinitable' is reserved for use by the compiler}}
func existential(_: any Deinitable) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
extension Int: Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
// expected-warning@-1 {{conformance of 'Int' to protocol 'Deinitable' was already stated in the type's module 'Swift'}}
protocol P: Deinitable {} // expected-error {{'Deinitable' is reserved for use by the compiler}}
protocol Q { associatedtype A: ~Copyable, ~Deinitable } // expected-error {{'Deinitable' is reserved for use by the compiler}}

func qualified(_: any Swift.Deinitable) {} // expected-error {{'Deinitable' is reserved for use by the compiler}}

// Existing noncopyable code is unaffected.
struct Resource: ~Copyable {
  deinit {}
}

func takeGeneric<T: ~Copyable>(_ t: consuming T) {}

func useResource() {
  takeGeneric(Resource())
}
