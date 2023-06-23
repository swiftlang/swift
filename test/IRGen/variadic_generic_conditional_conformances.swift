// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking | %FileCheck %s

protocol P {
  static func foobar()
}

struct G<each T> {}

extension G: P where repeat each T: P {
  static func foobar() {}
}

// CHECK-LABEL: define internal swiftcc void @"$s41variadic_generic_conditional_conformances1GVyxxQp_QPGAA1PA2aEP6foobaryyFZTW"(%swift.type* swiftself %0, %swift.type* %Self, i8** %SelfWitnessTable)

