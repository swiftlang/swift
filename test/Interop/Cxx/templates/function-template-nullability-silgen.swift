// RUN: %target-swift-emit-silgen %s -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

import FunctionTemplateNullability

// CHECK-LABEL: sil hidden [ossa] @{{.*}}testPointers
// CHECK: function_ref @{{.*}}nullableResult{{.*}} : $@convention(c) ({{.*}}) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: function_ref @{{.*}}nonnullResult{{.*}} : $@convention(c) ({{.*}}) -> UnsafeMutablePointer<Int32>
// CHECK: function_ref @{{.*}}unspecifiedResult{{.*}} : $@convention(c) ({{.*}}) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: function_ref @{{.*}}nullableParameter{{.*}} : $@convention(c) (Optional<UnsafeMutablePointer<Int32>>) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: function_ref @{{.*}}nullablePointerResult{{.*}} : $@convention(c) (UnsafeMutablePointer<Int32>) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: function_ref @{{.*}}nullableConstPointerResult{{.*}} : $@convention(c) (UnsafePointer<Int32>) -> Optional<UnsafePointer<Int32>>
// CHECK: function_ref @{{.*}}nonnullPointerResult{{.*}} : $@convention(c) (UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32>
func testPointers(p: UnsafeMutablePointer<CInt>, cp: UnsafePointer<CInt>) {
  let _: UnsafeMutablePointer<CInt>? = nullableResult(p)
  let _: UnsafeMutablePointer<CInt> = nonnullResult(p)
  let _: UnsafeMutablePointer<CInt> = unspecifiedResult(p)
  let _: UnsafeMutablePointer<CInt>? = nullableParameter(p)
  let _: UnsafeMutablePointer<CInt>? = nullablePointerResult(p)
  let _: UnsafePointer<CInt>? = nullableConstPointerResult(cp)
  let _: UnsafeMutablePointer<CInt> = nonnullPointerResult(p)
}
