// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=default %s | %FileCheck %s
// REQUIRES: PTRSIZE=64

// This test is meant to demonstrate how we once added imported C++ record with a class hierarchy incorrectly to aggregate lowerings
// and assert we don't make this mistake again.

import Fields

struct WrapperWithDerivedClangRecord {
  var leading: Double
  var value: DerivedFromOneField
}

// Previously, we would add storage that defines the layout of base classes (OneField in this case) twice,
// first at the wrong offset. This first erroneous adding of the base class storage for the value field
// would be added "on top of" the storage for the leading field, leading to an opaque lowering for the leading
// parts of the SwiftAggLowering. That lead the following suboptimal signature:

// define hidden swiftcc void @"$s4main36consumeWrapperWithDerivedClangRecordyyAA0cdefG0VF"(i64 %0, i32 %1)

func consumeWrapperWithDerivedClangRecord(
  _ value: WrapperWithDerivedClangRecord
) {}

// Base-class fields must be placed relative to the containing aggregate.
// CHECK: define hidden swiftcc void @"$s4main36consumeWrapperWithDerivedClangRecordyyAA0cdefG0VF"(double %0, i32 %1)
