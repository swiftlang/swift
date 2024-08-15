// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_value
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_value

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_value | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --check-prefix=X%target-ptrsize --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest


indirect enum MPEWithInts {
case stampA
case envelopeA(Int64)
case stampB
case envelopeB(Double)
case stampC
case envelopeC((Int32, Int32))
case stampD
case stampE
}

indirect enum SPEWithMPEPayload {
case payloadA(MPEWithInts)
case alsoA
case alsoB
case alsoC
case alsoD
}


reflect(enumValue: SPEWithMPEPayload.payloadA(.stampB))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_value.SPEWithMPEPayload)
// CHECK-NEXT: Value: .payloadA(.stampB)

doneReflecting()

// CHECK: Done.

