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
// Execute this file
//
// RUN: %target-build-swift %s -module-cache-path %t/mcp.swiftmod -I %t/include -L %t/bin -lcdecl_implementation -o %t/bin/swiftmod -module-name main
// RUN: %target-codesign %t/bin/swiftmod
// RUN: %target-run %t/bin/swiftmod | %FileCheck %s

//
// Execute again, without the swiftmodule this time
//
// RUN: mv %t/include/cdecl_implementation.swiftmodule %t/include/cdecl_implementation.swiftmodule.disabled
// RUN: %target-build-swift %s -module-cache-path %t/mcp.clangmod -I %t/include -L %t/bin -lcdecl_implementation -o %t/bin/clangmod -module-name main
// RUN: %target-codesign %t/bin/clangmod
// RUN: %target-run %t/bin/clangmod | %FileCheck %s

// REQUIRES: executable_test

import cdecl_implementation

implFunc(2023 - 34)
// CHECK: implFunc(1989)
