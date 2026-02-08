// Enum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-run-simple-swift(-enable-experimental-feature LiteralExpressions) | %FileCheck %s --dump-input=always

// CHECK: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 4
// CHECK-NEXT: 8
// CHECK-NEXT: 16
// CHECK-NEXT: 32
// CHECK-NEXT: 4
// CHECK-NEXT: 5
// CHECK-NEXT: 6

enum Powers: Int, CaseIterable {
    case zero = 1 << 0
    case one = 1 << 1
    case two = 1 << 2
    case three = 1 << 3
    case four = 1 << 4
    case five = 1 << 5
}
enum E1: Int, CaseIterable {
    case a = 2 + 2
    case b
    case c
}

for c in Powers.allCases {
    print(c.rawValue)
}
for c in E1.allCases {
    print(c.rawValue)
}
