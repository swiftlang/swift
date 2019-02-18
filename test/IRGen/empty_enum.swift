// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// CHECK: @"$s10empty_enum6JamaisOMf" =
//   CHECK-SAME: {{@"\$sytWV"|i8\*\* getelementptr inbounds \(%swift.enum_vwtable, %swift.enum_vwtable\* @"\$s10empty_enum6JamaisOWV", i32 0, i32 0\)}}

enum Jamais {}
