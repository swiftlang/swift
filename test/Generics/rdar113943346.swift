// RUN: %target-typecheck-verify-swift

struct G<T, U, V> {}

struct Foo<T> {}

// The first extension has the same generic signature as the
// second extension, because we drop the invalid requirement.
//
// But the rewrite system used for minimization with the first
// extension should not be installed in the rewrite context,
// because of this invalid requirement.

extension G where T == Foo<V.Bar>, U == Foo<Int> {}
// expected-error@-1 {{'Bar' is not a member type of type 'V'}}

extension G where U == Foo<Int> {
  func f() {
    print(T.self)
    print(U.self)
  }
}
