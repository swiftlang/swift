// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class C {}

// CHECK-LABEL: sil hidden @_TF7foreach3fooFGSaTCS_1CS0___T_
func foo(xx: [(C, C)]) {
  // CHECK: [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(C, C)>
  // CHECK: [[PAYLOAD:%.*]] = load [[PAYLOAD_ADDR]]
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: strong_release [[B]]
  // CHECK: strong_release [[A]]
  for (a, b) in xx {}
  // CHECK: [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(C, C)>
  // CHECK: [[PAYLOAD:%.*]] = load [[PAYLOAD_ADDR]]
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: strong_release [[B]]
  // CHECK: strong_release [[A]]
  for (a, _) in xx {}
  // CHECK: [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(C, C)>
  // CHECK: [[PAYLOAD:%.*]] = load [[PAYLOAD_ADDR]]
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: strong_release [[A]]
  // CHECK: strong_release [[B]]
  for (_, b) in xx {}
  // CHECK: [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(C, C)>
  // CHECK: [[PAYLOAD:%.*]] = load [[PAYLOAD_ADDR]]
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: strong_release [[B]]
  // CHECK: strong_release [[A]]
  for (_, _) in xx {}
  // CHECK: [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr {{%.*}} : $*Optional<(C, C)>
  // CHECK: [[PAYLOAD:%.*]] = load [[PAYLOAD_ADDR]]
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: strong_release [[B]]
  // CHECK: strong_release [[A]]
  for  _     in xx {}
}
