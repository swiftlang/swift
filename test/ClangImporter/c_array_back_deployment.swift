// Test that imported C arrays don't make the compiler reference InlineArray's
// nominal type descriptor when InlineArray isn't available at the deployment
// target. (rdar://190152649)

// REQUIRES: OS=macosx

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o - -primary-file %s > %t.ll
// RUN: %FileCheck --input-file=%t.ll %s --implicit-check-not='$ss11InlineArrayVMn'

import ctypes

// A generic use requires the field descriptor (and so the type references of
// the fields) of the struct.
@inline(never)
func consume<T>(_: T) {}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s{{[A-Za-z0-9_]*}}4testyyF"
public func test() {
  consume(StructWithPlainAndArrayFields())
  consume(StructWithAllHugeArrayFields())
  consume(StructWithNestedHugeArrayField())
  consume(StructWithHugeArrayOfHugeArrayPointers())
  consume(StructWithArrayOfVectors())
  consume(StructWithStaticArrays())
  consume(SmallArrayNewtype(rawValue: (1, 2, 3, 4)))
  consume(StructWithSmallArrayNewtypeField())
  consume(StructWithHugeArrayNewtypeField())
}
