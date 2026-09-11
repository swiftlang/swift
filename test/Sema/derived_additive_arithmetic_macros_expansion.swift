// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature AdditiveArithmeticDerivedConformances -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_AdditiveArithmeticDerivedConformances
// REQUIRES: swift_feature_DeriveConformancesViaMacros

struct Empty: AdditiveArithmetic {}

// CHECK: static var zero: Self {
// CHECK-NEXT:   Self()
// CHECK-NEXT: }

// CHECK: static func +(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self()
// CHECK-NEXT: }

// CHECK: static func -(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self()
// CHECK-NEXT: }

struct Int2: AdditiveArithmetic {
  var a: Int
  var b: Int
}

// CHECK: static var zero: Self {
// CHECK-NEXT:   Self(a: Int.zero, b: Int.zero)
// CHECK-NEXT: }

// CHECK: static func +(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(a: lhs.a + rhs.a, b: lhs.b + rhs.b)
// CHECK-NEXT: }

// CHECK: static func -(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(a: lhs.a - rhs.a, b: lhs.b - rhs.b)
// CHECK-NEXT: }

struct Generic<T: AdditiveArithmetic>: AdditiveArithmetic {
  var x: T
  var y: T
}

// CHECK: static var zero: Self {
// CHECK-NEXT:   Self(x: T.zero, y: T.zero)
// CHECK-NEXT: }

// CHECK: static func +(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
// CHECK-NEXT: }

// CHECK: static func -(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(x: lhs.x - rhs.x, y: lhs.y - rhs.y)
// CHECK-NEXT: }

struct WithRawIdentifiers: AdditiveArithmetic {
  var `default`: Int
  var `foo bar`: Double
}

// CHECK: static var zero: Self {
// CHECK-NEXT:   Self(`default`: Int.zero, `foo bar`: Double.zero)
// CHECK-NEXT: }

// CHECK: static func +(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(`default`: lhs.`default` + rhs.`default`, `foo bar`: lhs.`foo bar` + rhs.`foo bar`)
// CHECK-NEXT: }

// CHECK: static func -(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(`default`: lhs.`default` - rhs.`default`, `foo bar`: lhs.`foo bar` - rhs.`foo bar`)
// CHECK-NEXT: }

struct OnlyStoredProperties: AdditiveArithmetic {
  var x: Int
  static var scale: Int = 2
  var doubled: Int { x * 2 }
}

// CHECK: static var zero: Self {
// CHECK-NEXT:   Self(x: Int.zero)
// CHECK-NEXT: }

// CHECK: static func +(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(x: lhs.x + rhs.x)
// CHECK-NEXT: }

// CHECK: static func -(_ lhs: Self, _ rhs: Self) -> Self {
// CHECK-NEXT:   return Self(x: lhs.x - rhs.x)
// CHECK-NEXT: }
