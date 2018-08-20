// RUN: %target-typecheck-verify-swift -module-name test -primary-file %S/Inputs/require-layout-call-result-primary.swift


class C<T> {
  dynamic func broken() { } // expected-error{{'dynamic'}}
}

func bar<T, U: C<T>>(_ t: T, _ u: U) -> C<T> { return u }
