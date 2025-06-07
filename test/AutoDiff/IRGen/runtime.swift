// RUN: %target-swift-frontend -enable-builtin-module %s -emit-ir | %FileCheck %s

import Builtin
import _Differentiation

@_silgen_name("test_context_builtins_with_type")
func test_context_builtins_with_type<T>(t: T) {
  let context = Builtin.autoDiffCreateLinearMapContextWithType(T.self)
  let topLevelSubctxAddr = Builtin.autoDiffProjectTopLevelSubcontext(context)
  UnsafeMutableRawPointer(topLevelSubctxAddr).storeBytes(of: t, as: T.self)
  let newBuffer = Builtin.autoDiffAllocateSubcontextWithType(context, T.self)
  UnsafeMutableRawPointer(newBuffer).storeBytes(of: t, as: T.self)
}

// CHECK-LABEL: define{{.*}}@test_context_builtins_with_type(ptr noalias %0, ptr %T)
// CHECK: entry:
// CHECK:   [[CTX:%.*]] = call swiftcc ptr @swift_autoDiffCreateLinearMapContextWithType(ptr %T)
// CHECK:   call swiftcc ptr @swift_autoDiffProjectTopLevelSubcontext(ptr [[CTX]])
// CHECK:   [[BUF:%.*]] = call swiftcc ptr @swift_autoDiffAllocateSubcontextWithType(ptr [[CTX]], ptr %T)
