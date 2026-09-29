// RUN: %target-typecheck-verify-swift

// Without the 'ImplicitMemberOnFunctionType' feature enabled, a leading dot
// does not look through a function-typed parameter to the return type. This
// pins the pre-feature behavior so the change stays additive / source
// compatible: only code that is an error today starts to compile, and only
// when the feature is enabled.

struct Foo<T> {
  init(_ t: T) {}
  init<P>(_ t: (P) -> T) {}
}

enum X { case a; case b(Int) }

let a1 = Foo<X>(.a) // no-payload case still resolves directly against 'X'
let b1 = Foo<X>(.b) // expected-error {{member 'b' expects argument of type 'Int'}}
let b2 = Foo<X>(X.b) // spelling out the type is the pre-feature workaround
