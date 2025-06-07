// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-async-top-level -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -experimental-skip-non-inlinable-function-bodies -experimental-skip-non-inlinable-function-bodies-without-types -emit-silgen %s | %FileCheck -check-prefix=SKIPPED-FUNC-EMITTED %s

guard let x: Int = nil else { while true { } }

// CHECK-LABEL: sil hidden [ossa] @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> () {
// SKIPPED-FUNC-EMITTED-LABEL-NOT: sil hidden [ossa] @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> () {
func capturesX() {
  _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s18top_level_captures17transitiveCaptureyyF : $@convention(thin) (Int) -> () {
// CHECK: [[FUNC:%.*]] = function_ref @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> ()
// SKIPPED-FUNC-EMITTED-LABEL-NOT: sil hidden [ossa] @$s18top_level_captures17transitiveCaptureyyF : $@convention(thin) (Int) -> () {
// SKIPPED-FUNC-EMITTED-NOT: [[FUNC:%.*]] = function_ref @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> ()
func transitiveCapture() {
  capturesX()
}
