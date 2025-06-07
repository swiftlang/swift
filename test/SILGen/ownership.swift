// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-stdlib -module-name Swift -parse-as-library %s | %FileCheck %s

protocol Error {}

enum MyError : Error {
case case1
}

// CHECK: bb{{[0-9]+}}([[ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:   end_borrow {{.*}} : ${{.*}}() ->
// CHECK-NEXT:   destroy_value {{.*}} : ${{.*}}() ->
// CHECK-NEXT:   [[BORROWED_ERROR:%.*]] = begin_borrow [[ERROR]]
// CHECK-NEXT:   [[ERROR_SLOT:%.*]] = alloc_stack $any Error
// CHECK-NEXT:   [[COPIED_BORROWED_ERROR:%.*]] = copy_value [[BORROWED_ERROR]]
// CHECK-NEXT:   store [[COPIED_BORROWED_ERROR]] to [init] [[ERROR_SLOT]]
// CHECK-NEXT:   [[ERROR_SLOT_CAST_RESULT:%.*]] = alloc_stack $MyError
// CHECK-NEXT:   checked_cast_addr_br copy_on_success any Error in [[ERROR_SLOT]] : $*any Error to MyError in [[ERROR_SLOT_CAST_RESULT]] : $*MyError
func test1(f: () throws -> ()) throws {
  do {
    let _ = try f()
  } catch MyError.case1 {
  }
}
