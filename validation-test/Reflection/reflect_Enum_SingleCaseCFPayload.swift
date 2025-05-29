// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_SingleCaseCFPayload
// RUN: %target-codesign %t/reflect_Enum_SingleCaseCFPayload

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_SingleCaseCFPayload | %FileCheck %s --check-prefix=CHECK%target-ptrsize

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

// CHECK: Reflecting an object.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK: Type reference:
// CHECK-NEXT: (class reflect_Enum_SingleCaseCFPayload.ClassWithSingleCaseCFPayloadEnum)

// CHECK64: Type info:
// CHECK64-NEXT: (class_instance size=48 alignment=8 stride=48 num_extra_inhabitants=0 bitwise_takable=1 
// CHECK64-NEXT:  (field name=e1 offset=16 
// CHECK64-NEXT:   (single_payload_enum size=8 alignment=8 stride=8 num_extra_inhabitants=2147483646 bitwise_takable=1 
// CHECK64-NEXT:    (case name=some index=0 offset=0 
// CHECK64-NEXT:    (reference kind=strong refcounting=unknown)) 

// CHECK32: Type info:

reflect(enum: SingleCaseCFPayloadEnum.only(cfs1))

// CHECK: Reflecting an enum.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}

// CHECK: Type reference:
// CHECK-NEXT: (enum reflect_Enum_SingleCaseCFPayload.SingleCaseCFPayloadEnum)

// CHECK: Type info:
// CHECK-NEXT: (reference kind=strong refcounting=unknown) 

// CHECK: Mangled name: $s32reflect_Enum_SingleCaseCFPayload0cdeB0O 
// CHECK-NEXT: Demangled name: reflect_Enum_SingleCaseCFPayload.SingleCaseCFPayloadEnum 

// CHECK: Enum value:
// CHECK-NEXT: (reference kind=strong refcounting=unknown)

// CHECK: Mangled name: $s32reflect_Enum_SingleCaseCFPayload0cdeB0O 
// CHECK-NEXT: Demangled name: reflect_Enum_SingleCaseCFPayload.SingleCaseCFPayloadEnum 

doneReflecting()

// CHECK: Done.
