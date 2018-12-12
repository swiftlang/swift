// RUN: %target-swift-emit-silgen -enable-sil-ownership -parse-stdlib -parse-as-library  %s | %FileCheck %s
import Swift

// CHECK-LABEL: sil @{{.*}}apply{{.*}} : $@convention(thin) (@noescape @callee_guaranteed () -> Int)
// bb0(%0 : $@noescape @callee_guaranteed () -> Int):
//   [[B1:%.*]] = begin_borrow %0 : $@noescape @callee_guaranteed () -> Int
//   [[C1:%.*]] = copy_value %2 : $@noescape @callee_guaranteed () -> Int
//
//   The important part is that the call borrow's the function value -- we are
//   @callee_guaranteed.
//   [[B2:%.*]] = begin_borrow [[C1]] : $@noescape @callee_guaranteed () -> Int
//   [[R:%.*]] = apply [[B2]]() : $@noescape @callee_guaranteed () -> Int
//   end_borrow [[B2]] : $@noescape @callee_guaranteed () -> Int
//
//   destroy_value [[C1]] : $@noescape @callee_guaranteed () -> Int
//   end_borrow [[B1]] : $@noescape @callee_guaranteed () -> Int
//   destroy_value %0 : $@noescape @callee_guaranteed () -> Int
//   return [[R]] : $Int
public func apply(_ f : () -> Int) -> Int {
  return f()
}

// CHECK-LABEL: sil @{{.*}}test{{.*}} : $@convention(thin) () -> ()
// CHECK:   [[C1:%.*]] = function_ref @{{.*}}test{{.*}} : $@convention(thin) () -> Int
// CHECK:   [[C2:%.*]] = convert_function [[C1]] : $@convention(thin) () -> Int to $@convention(thin) @noescape () -> Int
// CHECK:   [[C3:%.*]] = thin_to_thick_function [[C2]] : $@convention(thin) @noescape () -> Int to $@noescape @callee_guaranteed () -> Int
// CHECK:   [[A:%.*]] = function_ref @{{.*}}apply{{.*}} : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> Int
// CHECK:   apply [[A]]([[C3]]) : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> Int
public func test() {
  let res = apply({ return 1 })
}
