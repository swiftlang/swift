// RUN: %target-swift-frontend %s -Osize -Xllvm -sil-print-types -Xllvm -sil-print-after=loadable-address -import-objc-header %S/Inputs/large_union.h -c -o %t/t.o 2>&1 | %FileCheck %s

public func test1(_ s: some_struct) -> some_struct {
  var copy = s
  copy.out.a = 1
  return copy
}
// CHECK: sil @$s1t5test1ySo11some_structaADF : $@convention(thin) (@in_guaranteed some_struct) -> @out some_struct {
// CHECK-NOT: unchecked_trivial_bitcast
// CHECK: unchecked_addr_cast {{.*}} : $*some_struct to $*some_struct.__Unnamed_struct_out
// CHECK-NOT: unchecked_trivial_bitcast
// CHECK: } // end sil function '$s1t5test1ySo11some_structaADF'

// CHECK: sil @$s1t5test2yySo11some_structazF : $@convention(thin) (@inout some_struct) -> () {
// CHECK-NOT: unchecked_trivial_bitcast
// CHECK: unchecked_addr_cast {{.*}} : $*some_struct to $*some_struct.__Unnamed_struct_out
// CHECK-NOT: unchecked_trivial_bitcast
// CHECK: } // end sil function '$s1t5test2yySo11some_structazF'

public func test2(_ s: inout some_struct) {
  s.out.a = 1
}
