// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class C {}


// CHECK-LABEL: sil hidden @_T07foreach13tupleElementsySayAA1CC_ADtGF
func tupleElements(_ xx: [(C, C)]) {
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for (a, b) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for (a, _) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: destroy_value [[A]]
  // CHECK: destroy_value [[B]]
  for (_, b) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for (_, _) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[A:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 0
  // CHECK: [[B:%.*]] = tuple_extract [[PAYLOAD]] : $(C, C), 1
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for  _     in xx {}
}

