// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

guard let x: Int = nil else { while true { } }

// CHECK-LABEL: sil hidden [ossa] @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> () {
func capturesX() {
  _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s18top_level_captures17transitiveCaptureyyF : $@convention(thin) (Int) -> () {
// CHECK: [[FUNC:%.*]] = function_ref @$s18top_level_captures0C1XyyF : $@convention(thin) (Int) -> ()
func transitiveCapture() {
  capturesX()
}
