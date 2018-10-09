// RUN: %target-swift-frontend -emit-sil -primary-file %s | %FileCheck %s

class C<T> {}
protocol P {
  func f()
}

// CHECK-LABEL: sil hidden [transparent] @$s27inline_subclass_existential3fooyyAA1P_AA1CCyxGXclF : $@convention(thin) <T> (@guaranteed C<T> & P) -> () {
// CHECK: open_existential_ref %0 : $C<T> & P to $@opened("{{.*}}") C<T> & P
// CHECK: return
@_transparent
func foo<T>(_ x: C<T> & P) {
  x.f()
}

// CHECK-LABEL: sil hidden @$s27inline_subclass_existential3baryyAA1P_AA1CCySiGXcF : $@convention(thin) (@guaranteed C<Int> & P) -> () {
// CHECK: open_existential_ref %0 : $C<Int> & P to $@opened("{{.*}}") C<Int> & P
// CHECK: return
func bar(_ x: C<Int> & P) {
  foo(x)
}
