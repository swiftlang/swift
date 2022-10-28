// RUN: %target-swift-frontend -enable-sil-opaque-values -parse-as-library -emit-sil -Onone %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s19opaque_values_Onone16generic_identity1txx_tlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : $*T, %1 : $*T):
// CHECK:   debug_value %1 : $*T, let, name "t", argno 1
// CHECK:   copy_addr %1 to [init] %0 : $*T
// CHECK-LABEL: } // end sil function '$s19opaque_values_Onone16generic_identity1txx_tlF'
func generic_identity<T>(t: T) -> T {
  return t
}


enum Maybe1<T : Equatable> {
  case nope
  case yep(T)
  // CHECK-LABEL: sil hidden @maybe1_compare {{.*}} {
  // CHECK:         [[LHS_ADDR:%[^,]+]] = alloc_stack [lexical] $Maybe1
  // CHECK:         [[RHS_ADDR:%[^,]+]] = alloc_stack [lexical] $Maybe1
  // CHECK:         switch_enum_addr [[LHS_ADDR]] : $*Maybe1<T>, case #Maybe1.yep!enumelt: [[L_YEP:bb[0-9]+]], case #Maybe1.nope!enumelt: {{bb[0-9]+}}
  // CHECK:       [[L_YEP]]:
  // CHECK:         unchecked_take_enum_data_addr [[LHS_ADDR]] : $*Maybe1<T>, #Maybe1.yep!enumelt
  // CHECK:         switch_enum_addr [[RHS_ADDR]] : $*Maybe1<T>, case #Maybe1.yep!enumelt: [[L_AND_R_YEP:bb[0-9]+]], default {{bb[0-9]+}}
  // CHECK:       [[L_AND_R_YEP]]:
  // CHECK:         unchecked_take_enum_data_addr [[RHS_ADDR]] : $*Maybe1<T>, #Maybe1.yep!enumelt
  // CHECK-LABEL: } // end sil function 'maybe1_compare'
  @_silgen_name("maybe1_compare")
  static func compare(_ lhs: Maybe1, _ rhs: Maybe1) -> Bool {
    switch (lhs, rhs) {
    case (.yep(let l), .yep(let r)):
      return l == r
    case (.nope, .nope):
      return true
    default:
      return false
    }
  }
}

enum Maybe2<T : Equatable> {
  case nope
  case yep(T, T)

  // CHECK-LABEL: sil hidden @maybe2_compare {{.*}} {
  // CHECK:         [[LHS_ADDR:%[^,]+]] = alloc_stack [lexical] $Maybe2<T>
  // CHECK:         [[RHS_ADDR:%[^,]+]] = alloc_stack [lexical] $Maybe2<T>
  // CHECK:         switch_enum_addr [[LHS_ADDR]] : $*Maybe2<T>, case #Maybe2.yep!enumelt: [[L_YEP:bb[0-9]+]], case #Maybe2.nope!enumelt: {{bb[0-9]+}}
  // CHECK:       [[L_YEP]]:
  // CHECK:         unchecked_take_enum_data_addr [[LHS_ADDR]] : $*Maybe2<T>, #Maybe2.yep!enumelt
  // CHECK:         switch_enum_addr [[RHS_ADDR]] : $*Maybe2<T>, case #Maybe2.yep!enumelt: [[R_YEP:bb[0-9]+]], default {{bb[0-9]+}}
  // CHECK:       [[L_AND_R_YEP]]:
  // CHECK:         unchecked_take_enum_data_addr [[RHS_ADDR]] : $*Maybe2<T>, #Maybe2.yep!enumelt
  // CHECK-LABEL: } // end sil function 'maybe2_compare'
  @_silgen_name("maybe2_compare")
  static func compare(_ lhs: Maybe2, _ rhs: Maybe2) -> Bool {
    switch (lhs, rhs) {
    case (.yep(let l, _), .yep(let r, _)):
      return l == r
    case (.nope, .nope):
      return true
    default:
      return false
    }
  }
}
