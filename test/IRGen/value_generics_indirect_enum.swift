// RUN: %target-swift-frontend -disable-availability-checking -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -O %s

// https://github.com/swiftlang/swift/issues/94098
//
// A recursive enum generic over a value parameter, with an InlineArray
// payload, used to emit an outlined destroy helper that took no metadata and
// tried to re-materialize 'Box<dimensions>' from scratch. The value generic
// argument was unavailable there, which produced a metadata accessor call with
// a null operand. The helper must instead receive the enum's metadata from its
// caller.

enum IndirectCaseBox<let dimensions: Int> {
  case point([dimensions of Double])
  indirect case box(IndirectCaseBox)
}

enum ArrayBox<let dimensions: Int> {
  case point([dimensions of Double])
  case boxes([ArrayBox])
}

enum IndirectArrayBox<let dimensions: Int> {
  case point([dimensions of Double])
  indirect case boxes([IndirectArrayBox])
}

// CHECK-DAG: define {{.*}} @"$s{{.*}}15IndirectCaseBoxO{{.*}}WOh"(ptr %0, ptr %"IndirectCaseBox<dimensions>")
// CHECK-DAG: define {{.*}} @"$s{{.*}}8ArrayBoxO{{.*}}WOh"(ptr %0, ptr %"ArrayBox<dimensions>")
// CHECK-DAG: define {{.*}} @"$s{{.*}}16IndirectArrayBoxO{{.*}}WOh"(ptr %0, ptr %"IndirectArrayBox<dimensions>")
