// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCaseCFPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCaseCFPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCaseCFPayload | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest
import Foundation

enum SingleCaseCFPayloadEnum {
case only(CFString)
}

let cfs1 = "1" as CFString
let cfs2 = "2" as CFString

class ClassWithSingleCaseCFPayloadEnum {
  var e1: SingleCaseCFPayloadEnum?
  var e2: SingleCaseCFPayloadEnum = .only(cfs1)
  var e3: SingleCaseCFPayloadEnum? = .some(.only(cfs2))
  var e4: SingleCaseCFPayloadEnum??
}

reflect(object: ClassWithSingleCaseCFPayloadEnum())

// CHECK-64: Reflecting an object.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (class reflect_Enum_SingleCaseCFPayload.ClassWithSingleCaseCFPayloadEnum)
// CHECK-64: Type info:
// CHECK-64: <null type info>

// CHECK-32: Reflecting an object.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (class reflect_Enum_SingleCaseCFPayload.ClassWithSingleCaseCFPayloadEnum)
// CHECK-32: Type info:
// CHECK-32: <null type info>

reflect(enum: SingleCaseCFPayloadEnum.only(cfs1))

// CHECK-64: Reflecting an enum.
// CHECK-64: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-64: Type reference:
// CHECK-64: (enum reflect_Enum_SingleCaseCFPayload.SingleCaseCFPayloadEnum)
// CHECK-64: Type info:
// CHECK-64: <null type info>
// CHECK-64: Enum value:
// CHECK-64: <null type info>

// CHECK-32: Reflecting an enum.
// CHECK-32: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-32: Type reference:
// CHECK-32: (enum reflect_Enum_SingleCaseCFPayload.SingleCaseCFPayloadEnum)
// CHECK-32: Type info:
// CHECK-32: <null type info>
// CHECK-32: Enum value:
// CHECK-32: <null type info>

doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.
