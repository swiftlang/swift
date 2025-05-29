// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_values8
// RUN: %target-codesign %t/reflect_Enum_values8

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_values8 | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK%target-ptrsize --check-prefix=CHECKALL --dump-input=fail %add_num_extra_inhabitants

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

enum B {
case a(Int)
case b(Float)
}

enum A_Opt {
case a(B?)
case other(Int)
}

reflect(enumValue: A_Opt.a(.none))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values8.A_Opt)
// CHECKALL-NEXT: Value: .a(.none)

reflect(enumValue: A_Opt.a(.a(0)))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values8.A_Opt)
// CHECKALL-NEXT: Value: .a(.some(.a(_)))

reflect(enumValue: A_Opt.a(.b(0.0)))

// CHECKALL: Reflecting an enum value.
// CHECKALL-NEXT: Type reference:
// CHECKALL-NEXT: (enum reflect_Enum_values8.A_Opt)
// CHECKALL-NEXT: Value: .a(.some(.b(_)))

doneReflecting()

// CHECKALL: Done.

