// RUN: %target-typecheck-verify-swift -typecheck -debug-constraints %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

// Make sure that the interpolation segments get placed into separate connected
// components.
// The literal segments, however, are in one component.
// CHECK: ---Connected components---
// CHECK-NEXT:  0: $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}}
// CHECK-NEXT:  1: $T{{[0-9]+}}
// CHECK-NEXT:  2: $T{{[0-9]+}}
// CHECK-NEXT:  3: $T{{[0-9]+}}
// CHECK-NEXT:  4: $T{{[0-9]+}}

// CHECK: (solving component #0
// CHECK-NEXT: literal=3 involves_type_vars bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

// CHECK: (solving component #1
// CHECK-NEXT: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #2
// CHECK-NEXT: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #3
// CHECK-NEXT: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #4
// CHECK-NEXT: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

_ = "\(1), \(2), \(3), \(4)"
