// RUN: %target-swift-frontend -sil-enable-typed-boxes -emit-silgen %s | FileCheck %s

func foo(var x: Int) -> () -> Int {
  return { x }
}
// CHECK-LABEL: sil shared @_TFF19capture_typed_boxes3fooFSiFT_SiU_FT_Si : $@convention(thin) (@owned @box Int, @inout Int) -> Int {
// CHECK:       bb0(%0 : $@box Int, %1 : $*Int):
