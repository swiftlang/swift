// RUN: %target-swift-frontend  %s -O -emit-sil | %FileCheck %s

// REQUIRES: optimized_stdlib

extension Sequence where Element: BinaryInteger {

  @specialized(where Self == [Int])
  public func sum() -> Double {
    reduce(0) { $0 + Double($1) }
  }
// CHECK: sil @$sST20prespecialize_publicSz7ElementRpzrlE3sumSdyF : $@convention(method) <Self where Self : Sequence, Self.Element : BinaryInteger> (@in_guaranteed Self) -> Double {
// CHECK: bb0(%0 : $*Self):
// CHECK:   [[T1:%.*]] = metatype $@thick Self.Type
// CHECK:   [[T2:%.*]] = metatype $@thick Array<Int>.Type
// CHECK:   [[T3:%.*]] = unchecked_trivial_bit_cast [[T1]] to $Builtin.Word
// CHECK:   [[T4:%.*]] = unchecked_trivial_bit_cast [[T2]] to $Builtin.Word
// CHECK:   [[T5:%.*]] = builtin "cmp_eq_Word"([[T3]], [[T4]]) : $Builtin.Int1
// CHECK:   cond_br [[T5]], bb3, bb1

// CHECK: bb1:
// CHECK:   [[T6:%.*]] = function_ref @$sSTsE6reduceyqd__qd___qd__qd___7ElementQztKXEtKlF
// CHECK:   apply {{.*}} [[T6]]<Self, Double>(
// CHECK:   br bb2(

// CHECK: bb2({{.*}} : $Double):
// CHECK:   return

// CHECK: bb3:
// CHECK:   %25 = unchecked_addr_cast %0 to $*Array<Int>
}
