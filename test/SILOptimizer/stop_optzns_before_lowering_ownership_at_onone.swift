// RUN: %target-swift-frontend %s -emit-sil -sil-stop-optzns-before-lowering-ownership | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-sil | %FileCheck -check-prefix=NEGATIVE %s

// CHECK: sil hidden [ossa] @$s46stop_optzns_before_lowering_ownership_at_onone4testyyF : $@convention(thin) () -> () {
// NEGATIVE: sil hidden @$s46stop_optzns_before_lowering_ownership_at_onone4testyyF : $@convention(thin) () -> () {
func test() {

}
