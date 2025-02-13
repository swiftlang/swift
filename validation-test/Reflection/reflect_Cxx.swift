// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// %target-clangxx -c %S/Inputs/Cxx/pimpl.cpp -I %S/Inputs/Cxx -o %t/pimpl.o
// RUN: %target-build-swift -lswiftSwiftReflectionTest %t/reflect_Cxx.swift -I %t -cxx-interoperability-mode=default -o %t/reflect_Cxx
// RUN: %target-codesign %t/reflect_Cxx

// RUN: %target-run %target-swift-reflection-test %t/reflect_Cxx | tee /dev/stderr | %FileCheck %s

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

//--- module.modulemap

module CxxStruct {
  header "CxxStruct.h"
  requires cplusplus
  export *
}

//--- CxxStruct.h

struct CxxStructA {
private:
  int i;
};

inline CxxStructA createCxxStructA() {
  return CxxStructA();
}

//--- reflect_Cxx.swift

import SwiftReflectionTest
import CxxStruct

// Trigger type metadata to be emitted by conforming C++ types to a Swift protocol.
protocol MyProto {}
extension CxxStructA : MyProto {}

let a = createCxxStructA()
reflect(any: a)

// CHECK: Reflecting an existential.
// CHECK-NEXT: Instance pointer in child address space: 0x{{[0-9a-fA-F]+}}
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (struct __C.CxxStructA)

// CHECK: Type info:
// CHECK-NEXT: (builtin size=4 alignment=4 stride=4 num_extra_inhabitants=0 bitwise_takable=1)
// CHECK-NEXT: Mangled name: $sSo10CxxStructAV
// CHECK-NEXT: Demangled name: __C.CxxStructA

// CHECK: Start of instance data: 0x{{[0-9a-fA-F]+}}

doneReflecting()

// CHECK: Done.
