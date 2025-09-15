
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name generic_tuples -parse-as-library %s | %FileCheck %s


func dup<T>(_ x: T) -> (T, T) { return (x,x) }
// CHECK-LABEL:      sil hidden [ossa] @$s14generic_tuples3dup{{[_0-9a-zA-Z]*}}F
// CHECK:      ([[RESULT_0:%.*]] : $*T, [[RESULT_1:%.*]] : $*T, [[XVAR:%.*]] : $*T):
// CHECK-NEXT: debug_value [[XVAR]] : $*T, let, name "x", {{.*}} expr op_deref
// CHECK-NEXT: copy_addr [[XVAR]] to [init] [[RESULT_0]]
// CHECK-NEXT: copy_addr [[XVAR]] to [init] [[RESULT_1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]]

// <rdar://problem/13822463>
// Specializing a generic function on a tuple type changes the number of
// SIL parameters, which caused a failure in the ownership conventions code.

struct Blub {}
// CHECK-LABEL: sil hidden [ossa] @$s14generic_tuples3foo{{[_0-9a-zA-Z]*}}F
func foo<T>(_ x: T) {}
// CHECK-LABEL: sil hidden [ossa] @$s14generic_tuples3bar{{[_0-9a-zA-Z]*}}F
func bar(_ x: (Blub, Blub)) { foo(x) }


// rdar://26279628
//   A type parameter constrained to be a concrete type must be handled
//   as that concrete type throughout SILGen.  That's especially true
//   if it's constrained to be a tuple.

protocol HasAssoc {
  associatedtype A
}
extension HasAssoc where A == (Int, Int) {
  func returnTupleAlias() -> A {
    return (0, 0)
  }
}
// CHECK-LABEL: sil hidden [ossa] @$s14generic_tuples8HasAssocPAASi_Sit1ARtzrlE16returnTupleAliasSi_SityF : $@convention(method) <Self where Self : HasAssoc, Self.A == (Int, Int)> (@in_guaranteed Self) -> (Int, Int) {
// CHECK:       return {{.*}} : $(Int, Int)
