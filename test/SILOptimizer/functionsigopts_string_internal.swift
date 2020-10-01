// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | %FileCheck %s

private var _storage: String? = nil

// CHECK-LABEL: sil hidden [noinline] @$s31functionsigopts_string_internal10setStorage2toySS_tF : $@convention(thin) (@guaranteed String) -> () {
// CHECK-LABEL: } // end sil function '$s31functionsigopts_string_internal10setStorage2toySS_tF'
@inline(never)
func setStorage(to newValue: String) {
  _storage = newValue
}

// CHECK-LABEL: sil hidden @$s31functionsigopts_string_internal12setStorageToyySSF : $@convention(thin) (@guaranteed String) -> () {
// CHECK: bb{{[0-9]+}}([[STRING:%.*]] : $String):
// CHECK: [[FUNCTION:%.*]] = function_ref @$s31functionsigopts_string_internal10setStorage2toySS_tF
// CHECK: apply [[FUNCTION]]([[STRING]])
// CHECK-LABEL: } // end sil function '$s31functionsigopts_string_internal12setStorageToyySSF'
func setStorageTo(_ newValue: String) {
  setStorage(to: newValue)
}

