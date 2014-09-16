// RUN: %swift -emit-silgen %s | FileCheck %s

protocol UID {
    func uid() -> Int
}

protocol ObjectUID : class, UID {}

class Base {}

// CHECK-LABEL: sil @_TF25protocol_class_refinement12getObjectUIDUS_9ObjectUID__FQ_Si
// CHECK-NOT:     strong_retain %0
// CHECK:         [[MATERIALIZED_SELF:%.*]] = alloc_stack $T
// CHECK-NOT:     strong_retain %0
// CHECK:         store %0 to [[MATERIALIZED_SELF]]
// CHECK:         [[WITNESS:%.*]] = witness_method
// CHECK:         apply [[WITNESS]]<T>([[MATERIALIZED_SELF]]#1)
// CHECK-NOT:     destroy_addr [[MATERIALIZED_SELF]]
// CHECK:         dealloc_stack [[MATERIALIZED_SELF]]
func getObjectUID<T: ObjectUID>(x: T) -> Int {
  return x.uid()
}

// CHECK-LABEL: sil @_TF25protocol_class_refinement16getBaseObjectUIDUS_3UID__FQ_Si
// CHECK-NOT:     strong_retain %0
// CHECK:         [[MATERIALIZED_SELF:%.*]] = alloc_stack $T
// CHECK-NOT:     strong_retain %0
// CHECK:         store %0 to [[MATERIALIZED_SELF]]
// CHECK:         [[WITNESS:%.*]] = witness_method
// CHECK:         apply [[WITNESS]]<T>([[MATERIALIZED_SELF]]#1)
// CHECK-NOT:     destroy_addr [[MATERIALIZED_SELF]]
// CHECK:         dealloc_stack [[MATERIALIZED_SELF]]
func getBaseObjectUID<T: UID where T: Base>(x: T) -> Int {
  return x.uid()
}
