// RUN: %swift -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil @_TF14metatype_casts6t_is_uU___FT_Sb
// CHECK:         checked_cast_br archetype_to_archetype {{.*}} $@thick T.Type to $@thick U.Type
func t_is_u<T, U>() -> Bool {
  return T.self is U.Type
}
