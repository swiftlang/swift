// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
public protocol CP: AnyObject {}

public func classExistential(_: __owned CP?) {}
// CHECK-LABEL: define{{.*}}16classExistential
// CHECK-NOT:     Os"
// CHECK-NOT:     Oe"
// CHECK:         swift_{{.*}}elease

public func classExistential2(_: __owned CP??) {}
// CHECK-LABEL: define{{.*}}17classExistential2
// CHECK:         2CP_pSgSgWOe"

// TODO: 32-bit String layout still triggers an out-of-line constructor
public func string(_: __owned String?) {}
// CHECK-64-LABEL: define{{.*}}6string
// CHECK-64-NOT:     Os"
// CHECK-64-NOT:     Oe"
// CHECK-64:         swift_{{.*}}elease

public func array(_: __owned [String]?) {}
// CHECK-LABEL: define{{.*}}5array
// CHECK-NOT:     Os"
// CHECK-NOT:     Oe"
// CHECK:         swift_{{.*}}elease

public func dict(_: __owned [String: String]?) {}
// CHECK-LABEL: define{{.*}}4dict
// CHECK-NOT:     Os"
// CHECK-NOT:     Oe"
// CHECK:         swift_{{.*}}elease

// Should get its extra inhabitant from x. The other "junk" fields are trivial
// so we can avoid branching the value witness operations.
public struct TrivialJunkFields {
  var w: Int
  var x: [String]
  var y: Int
}

public func trivialJunkFields(_: __owned TrivialJunkFields?) {}
// CHECK-LABEL: define{{.*}}17trivialJunkFields
// CHECK-NOT:     Os"
// CHECK-NOT:     Oe"
// CHECK:         swift_{{.*}}elease


// Should get its extra inhabitants from `y`. We can't avoid branching
// in the value witness because the bits of `x` are undefined if an enum
// is used.
public struct NontrivialJunkFields {
  var w: Int
  var x: [String]
  var y: [String]
}

public func nontrivialJunkFields(_: __owned NontrivialJunkFields?) {}
// CHECK-LABEL: define{{.*}}20nontrivialJunkFields
// CHECK:     20NontrivialJunkFieldsVSgW{{Os|Oe}}"
