// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | %FileCheck %s

private var _storage: String? = nil


// CHECK-LABEL: sil [noinline] @$s29functionsigopts_string_public10setStorage2toySS_tF : $@convention(thin) (@guaranteed String) -> () {
// CHECK-LABEL: } // end sil function '$s29functionsigopts_string_public10setStorage2toySS_tF'
@inline(never)
public func setStorage(to newValue: String) {
  _storage = newValue
}

// CHECK-LABEL: sil @$s29functionsigopts_string_public12setStorageToyySSF : $@convention(thin) (@guaranteed String) -> () {
// CHECK: bb{{[0-9]+}}([[STRING:%.*]] : $String):
// CHECK: [[FUNCTION:%.*]] = function_ref @$s29functionsigopts_string_public10setStorage2toySS_tF
// CHECK: apply [[FUNCTION]]([[STRING]])
// CHECK-LABEL: } // end sil function '$s29functionsigopts_string_public12setStorageToyySSF'
public func setStorageTo(_ newValue: String) {
  setStorage(to: newValue)
}

