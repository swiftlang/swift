// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class C {}


// CHECK-LABEL: sil hidden @_T07foreach13tupleElementsySayAA1CC_ADtGF
func tupleElements(_ xx: [(C, C)]) {
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[BORROWED_PAYLOAD:%.*]] = begin_borrow [[PAYLOAD]]
  // CHECK: [[A:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 0
  // CHECK: [[COPY_A:%.*]] = copy_value [[A]]
  // CHECK: [[B:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 1
  // CHECK: [[COPY_B:%.*]] = copy_value [[B]]
  // CHECK: end_borrow [[BORROWED_PAYLOAD]] from [[PAYLOAD]]
  // CHECK: destroy_value [[PAYLOAD]]
  // CHECK: destroy_value [[COPY_B]]
  // CHECK: destroy_value [[COPY_A]]
  for (a, b) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[BORROWED_PAYLOAD:%.*]] = begin_borrow [[PAYLOAD]]
  // CHECK: [[A:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 0
  // CHECK: [[COPY_A:%.*]] = copy_value [[A]]
  // CHECK: [[B:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 1
  // CHECK: [[COPY_B:%.*]] = copy_value [[B]]
  // CHECK: destroy_value [[COPY_B]]
  // CHECK: end_borrow [[BORROWED_PAYLOAD]] from [[PAYLOAD]]
  // CHECK: destroy_value [[PAYLOAD]]
  // CHECK: destroy_value [[COPY_A]]
  for (a, _) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[BORROWED_PAYLOAD:%.*]] = begin_borrow [[PAYLOAD]]
  // CHECK: [[A:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 0
  // CHECK: [[COPY_A:%.*]] = copy_value [[A]]
  // CHECK: [[B:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 1
  // CHECK: [[COPY_B:%.*]] = copy_value [[B]]
  // CHECK: destroy_value [[COPY_A]]
  // CHECK: end_borrow [[BORROWED_PAYLOAD]] from [[PAYLOAD]]
  // CHECK: destroy_value [[PAYLOAD]]
  // CHECK: destroy_value [[COPY_B]]
  for (_, b) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[BORROWED_PAYLOAD:%.*]] = begin_borrow [[PAYLOAD]]
  // CHECK: [[A:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 0
  // CHECK: [[COPY_A:%.*]] = copy_value [[A]]
  // CHECK: [[B:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 1
  // CHECK: [[COPY_B:%.*]] = copy_value [[B]]
  // CHECK: destroy_value [[COPY_B]]
  // CHECK: destroy_value [[COPY_A]]
  // CHECK: end_borrow [[BORROWED_PAYLOAD]] from [[PAYLOAD]]
  // CHECK: destroy_value [[PAYLOAD]]
  for (_, _) in xx {}
  // CHECK: [[PAYLOAD:%.*]] = unchecked_enum_data {{%.*}} : $Optional<(C, C)>, #Optional.some!enumelt.1
  // CHECK: [[BORROWED_PAYLOAD:%.*]] = begin_borrow [[PAYLOAD]]
  // CHECK: [[A:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 0
  // CHECK: [[COPY_A:%.*]] = copy_value [[A]]
  // CHECK: [[B:%.*]] = tuple_extract [[BORROWED_PAYLOAD]] : $(C, C), 1
  // CHECK: [[COPY_B:%.*]] = copy_value [[B]]
  // CHECK: destroy_value [[COPY_B]]
  // CHECK: destroy_value [[COPY_A]]
  // CHECK: end_borrow [[BORROWED_PAYLOAD]] from [[PAYLOAD]]
  // CHECK: destroy_value [[PAYLOAD]]
  for  _     in xx {}
}

