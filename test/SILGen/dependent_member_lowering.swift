// RUN: %swift -emit-silgen %s | FileCheck %s

protocol P {
  typealias A

  func f(x: A)
}
struct Foo<T>: P {
  typealias A = T.Type

  func f(t: T.Type) {}
  // CHECK-LABEL: sil hidden @_TTWU__GV25dependent_member_lowering3FooQ__S_1PS_FS1_1fUS1__U__fQPS1_FQS2_1AT_ : $@cc(witness_method) @thin <T> (@in @thick T.Type, @in_guaranteed Foo<T>) -> ()
  // CHECK:       bb0(%0 : $*@thick T.Type, %1 : $*Foo<T>):
}
struct Bar<T>: P {
  typealias A = Int -> T

  func f(t: Int -> T) {}
  // CHECK-LABEL: sil hidden @_TTWU__GV25dependent_member_lowering3BarQ__S_1PS_FS1_1fUS1__U__fQPS1_FQS2_1AT_ : $@cc(witness_method) @thin <T> (@in @callee_owned (@out T, @in Int) -> (), @in_guaranteed Bar<T>) -> ()
  // CHECK:       bb0(%0 : $*@callee_owned (@out T, @in Int) -> (), %1 : $*Bar<T>):
}
