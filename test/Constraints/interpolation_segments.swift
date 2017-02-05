// RUN: %target-typecheck-verify-swift -typecheck -debug-constraints %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

// Make sure that the interpolation segments get placed into separate connected
// components.
// CHECK: ---Connected components---
// CHECK-NEXT:  0: 
// CHECK-NEXT:  1: 
// CHECK-NEXT:  2: 
// CHECK-NEXT:  3: 
// CHECK-NEXT:  4: 
// CHECK-NEXT:  5: 
// CHECK-NEXT:  6: 
// CHECK-NEXT:  7: 
// CHECK-NEXT:  8: 
// CHECK-NEXT:  9:

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByIntegerLiteral) Int)

// CHECK: (solving component #
// CHECK: literal=3 bindings=(subtypes of) (default from ExpressibleByStringLiteral) String)

_ = "\(1), \(2), \(3), \(4)"
