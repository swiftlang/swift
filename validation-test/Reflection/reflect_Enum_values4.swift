// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values4
// RUN: %target-codesign %t/reflect_Enum_values4

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values4 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

enum NonPayloadEnum {
  case one
  case two
}

enum SmallMultipayloadEnum {
  case empty
  case a(NonPayloadEnum)
  case b(NonPayloadEnum)
}

reflect(enumValue: SmallMultipayloadEnum.b(.two))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values4.SmallMultipayloadEnum)
// CHECKALL-NEXT: Value: .b(.two)

reflect(enumValue: SmallMultipayloadEnum.empty)

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values4.SmallMultipayloadEnum)
// CHECKALL-NEXT: Value: .empty

reflect(enumValue: SmallMultipayloadEnum.a(.one))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values4.SmallMultipayloadEnum)
// CHECKALL-NEXT: Value: .a(.one)


doneReflecting()

// CHECKALL: Done.

