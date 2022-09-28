// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/large-structs-pass-return-indirect-in-c.swift -typecheck -module-name Structs -clang-header-expose-decls=all-public -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clang -c %s -I %t -o %t/swift-structs-execution.o
// RUN: %target-interop-build-swift %S/large-structs-pass-return-indirect-in-c.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "structs.h"

int main() {
  // printStructSeveralI64(returnNewStructSeveralI64(42))
  struct Structs_StructSeveralI64 structSeveralI64;
  $s7Structs25returnNewStructSeveralI641iAA0deF0Vs5Int64V_tF(&structSeveralI64, 42);
  $s7Structs21printStructSeveralI64yyAA0cdE0VF(&structSeveralI64);
// CHECK: StructSeveralI64.1 = 42, .2 = 0, .3 = -17, .4 = 12345612, .5 = -65535

  // printStructSeveralI64(passThroughStructSeveralI64(581, returnNewStructSeveralI64(42), 5.0))
  struct Structs_StructSeveralI64 structSeveralI64_copy;
  $s7Structs27passThroughStructSeveralI641i_1jAA0deF0Vs5Int64V_AFSftF(&structSeveralI64_copy,
    581, &structSeveralI64, 5.0f);
  $s7Structs21printStructSeveralI64yyAA0cdE0VF(&structSeveralI64_copy);
// CHECK-NEXT: StructSeveralI64.1 = 42, .2 = 581, .3 = -17, .4 = -12345612, .5 = -65530
  return 0;
}
