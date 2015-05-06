// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TF14metatype_casts6t_is_uU___FT_Sb
// CHECK:         checked_cast_br {{.*}} $@thick T.Type to $@thick U.Type
func t_is_u<T, U>() -> Bool {
  return T.self is U.Type
}

// CHECK-LABEL: sil hidden @_TF14metatype_casts8int_is_tU__FT_TSbGSqMQ__MQ__
func int_is_t<T>() -> (Bool, T.Type?, T.Type) {
  // CHECK: checked_cast_br {{%.*}} : $@thick Int.Type to $@thick T.Type
  // CHECK: checked_cast_br {{%.*}} : $@thick Int.Type to $@thick T.Type
  // CHECK: unconditional_checked_cast {{%.*}} : $@thick Int.Type to $@thick T.Type
  return (Int.self is T.Type, Int.self as? T.Type, Int.self as! T.Type)
}

// CHECK-LABEL: sil hidden @_TF14metatype_casts8t_is_intU__FT_TSbGSqMSi_MSi_
func t_is_int<T>() -> (Bool, Int.Type?, Int.Type) {
  // CHECK: checked_cast_br {{%.*}} : $@thick T.Type to $@thick Int.Type
  // CHECK: checked_cast_br {{%.*}} : $@thick T.Type to $@thick Int.Type
  // CHECK: unconditional_checked_cast {{%.*}} : $@thick T.Type to $@thick Int.Type
  return (T.self is Int.Type, T.self as? Int.Type, T.self as! Int.Type)
}


