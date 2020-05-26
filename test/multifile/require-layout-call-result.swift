// RUN: %target-swift-frontend -emit-ir -module-name test %s -primary-file %S/Inputs/require-layout-call-result-primary.swift


class C<T> {
  func broken() { }
}

func bar<T, U: C<T>>(_ t: T, _ u: U) -> C<T> { return u }
