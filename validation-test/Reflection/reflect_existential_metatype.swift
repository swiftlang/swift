// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_existential_metatype
// RUN: %target-codesign %t/reflect_existential_metatype
// RUN: %target-run %target-swift-reflection-test %t/reflect_existential_metatype | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

// An existential metatype `P.Type` must reflect as `.Type`, not `.Protocol`
// (`P.Protocol` is the distinct protocol metatype).

import SwiftReflectionTest

protocol MyProto {}

class Box<T> {}

reflect(any: Box<any MyProto.Type>())

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (bound_generic_class {{.*}}Box
// CHECK: (existential_metatype
// CHECK: Demangled name: {{.*}}Box<{{.*}}MyProto.Type>

doneReflecting()
