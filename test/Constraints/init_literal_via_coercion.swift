// RUN: %target-swift-frontend %s -typecheck -debug-constraints -swift-version 4 2>%t.err
// RUN: %FileCheck %s < %t.err

// SE-0213. `UInt32(0)` and similar expressions that get transformed into
// `0 as <#Type#>` should get literal bound early via equality constraint.

// CHECK: ---Constraint solving at [{{.*}}:12:1 - line:12:13]---
// CHECK: (integer_literal_expr type="[[LITERAL_VAR:\$T[0-9]+]]" {{.*}}
// CHECK: Type Variables:
// CHECK: [[LITERAL_VAR]] as UInt32 {{.*}}
// CHECK-NOT: disjunction (remembered) \[\[locator@{{.*}} [Coerce@{{.*}}\]\]]:
_ = UInt32(0)

// CHECK: ---Constraint solving at [{{.*}}22:1 - line:22:13]---
// CHECK: (coerce_expr implicit type="[[CAST_TYPE:\$T[0-9]+]]" {{.*}}
// CHECK-NEXT: (nil_literal_expr type="[[LITERAL_VAR:\$T[0-9]+]]" {{.*}}
// CHECK: Type Variables:
// CHECK: [[LITERAL_VAR]] as Int? {{.*}}
// CHECK: disjunction (remembered) {{.*}}
// CHECK-NEXT: >  [favored]  [[CAST_TYPE]] bind Int?
// CHECK-NEXT: >             [[CAST_TYPE]] bind Int
_ = Int!(nil)
