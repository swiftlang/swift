// UNSUPPORTED: OS=windows-msvc
// static library is not well supported yet on Windows

// REQUIRES: lld_lto

// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-library -static -lto=llvm-full -emit-module %S/Inputs/lto/module1.swift -working-directory %t
// RUN: %target-swiftc_driver -lto=llvm-full %s -I%t -L%t -lmodule1 -module-name main -o %t/main
// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s

// CHECK-NOT: _$s7module120unusedPublicFunctionyyF

import module1
