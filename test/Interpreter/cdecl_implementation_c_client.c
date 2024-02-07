// Test doesn't pass on devices or against OS dylibs (rdar://101543397)
// UNSUPPORTED: remote_run
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// RUN: %empty-directory(%t/include)
// RUN: %empty-directory(%t/bin)

//
// Build libcdecl_implementation
//
// RUN: %empty-directory(%t/include/cdecl_implementation.swiftmodule)
// RUN: cp %S/Inputs/cdecl_implementation.modulemap %t/include/module.modulemap
// RUN: cp %S/Inputs/cdecl_implementation.h %t/include/
// RUN: %target-build-swift-dylib(%t/bin/%target-library-name(cdecl_implementation)) -emit-module-path %t/include/cdecl_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name cdecl_implementation -I %t/include -import-underlying-module %S/cdecl_implementation.swift

//
// Build and execute this file
//
// RUN: %target-clang -x c -fmodules -isysroot %sdk -I %t/include -L %t/bin -lcdecl_implementation -o %t/bin/cdecl_implementation_c_client %s
// RUN: %target-codesign %t/bin/cdecl_implementation_c_client
// RUN: %target-run-additional-libs(%t/bin) %t/bin/cdecl_implementation_c_client 2>&1 | %FileCheck %s

// REQUIRES: executable_test

#include <cdecl_implementation.h>
#include <stdlib.h>

int main() {
  implFunc(2023 - 34);
  // CHECK: implFunc(1989)

  return 0;
}
