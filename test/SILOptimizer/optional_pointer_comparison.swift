// RUN: %target-swift-frontend -module-name test -primary-file %s -O -emit-sil | %FileCheck %s

public class C {}

// A class reference compared against an optional folds into a single pointer comparison.
// CHECK-LABEL: sil {{.*}}3cmpySbAA1CC
// CHECK-NOT:     switch_enum
// CHECK:         [[A:%.*]] = ref_to_raw_pointer %0
// CHECK:         [[O:%.*]] = unchecked_trivial_bit_cast %1
// CHECK:         builtin "cmp_eq_RawPointer"([[A]], [[O]])
// CHECK:       } // end sil function
public func cmp(_ a: C, _ b: C?) -> Bool {
  return a === b
}

// A raw pointer folds too, now that its payload `unchecked_enum_data` is forwarded.
// CHECK-LABEL: sil {{.*}}rawPointerCmp
// CHECK-NOT:     switch_enum
// CHECK:         [[A:%.*]] = unchecked_trivial_bit_cast %0
// CHECK:         [[O:%.*]] = unchecked_trivial_bit_cast %1
// CHECK:         builtin "cmp_eq_RawPointer"([[A]], [[O]])
// CHECK:       } // end sil function
public func rawPointerCmp(_ a: UnsafeRawPointer, _ b: UnsafeRawPointer?) -> Bool {
  return a == b
}
