// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import LoggingFrts

func takesLargeStructWithRefCountedField(_ x: LargeStructWithRefCountedField) {
    var a = x
}

takesLargeStructWithRefCountedField(getStruct())
// CHECK:      RefCount: 1, message: Ctor
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: RefCount: 0, message: release
// CHECK-NEXT: RefCount: 0, message: Dtor

func takesLargeStructWithRefCountedFieldNested(_ x: LargeStructWithRefCountedFieldNested) {
    var a = x
}

takesLargeStructWithRefCountedFieldNested(getNestedStruct())
// CHECK:      RefCount: 1, message: Ctor
// CHECK-NEXT: RefCount: 2, message: retain
// CHECK-NEXT: RefCount: 1, message: release
// CHECK-NEXT: RefCount: 0, message: release
// CHECK-NEXT: RefCount: 0, message: Dtor
