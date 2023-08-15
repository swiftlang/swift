// RUN: %target-swift-frontend -parse-stdlib %s -emit-ir | %FileCheck %s

import Swift

@_silgen_name("test_context_builtins")
func test_context_builtins<T>(t: T) {
  let context = Builtin.autoDiffCreateLinearMapContext(T.self)
  let topLevelSubctxAddr = Builtin.autoDiffProjectTopLevelSubcontext(context)
  UnsafeMutableRawPointer(topLevelSubctxAddr).storeBytes(of: t, as: T.self)
  let newBuffer = Builtin.autoDiffAllocateSubcontext(context, T.self)
  UnsafeMutableRawPointer(newBuffer).storeBytes(of: t, as: T.self)
}

// CHECK-LABEL: define{{.*}}@test_context_builtins(ptr noalias nocapture %0, ptr %T)
// CHECK: entry:
// CHECK:   [[CTX:%.*]] = call swiftcc ptr @swift_autoDiffCreateLinearMapContext(ptr %T)
// CHECK:   call swiftcc ptr @swift_autoDiffProjectTopLevelSubcontext(ptr [[CTX]])
// CHECK:   [[BUF:%.*]] = call swiftcc ptr @swift_autoDiffAllocateSubcontext(ptr [[CTX]], ptr %T)
