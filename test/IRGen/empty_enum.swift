// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// CHECK: @"$S10empty_enum6JamaisOMf" =
//   CHECK-SAME: @"$SytWV"

enum Jamais {}
