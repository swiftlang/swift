// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// CHECK-LABEL: @"$s36generic_metadata_fixed_known_vwtable6StroctVMP" =
// CHECK-SAME:    $sytWV
struct Stroct<T> {}

// CHECK-LABEL: @"$s36generic_metadata_fixed_known_vwtable4EnomOMP" =
// CHECK-SAME:    $sytWV
enum Enom<T> {}
