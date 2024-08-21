// Test doesn't pass on all platforms (rdar://101543397)
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)
// RUN: %empty-directory(%t/frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule)
// RUN: %empty-directory(%t/frameworks/cdecl_implementation.framework/Headers)
// RUN: cp %S/Inputs/cdecl_implementation.modulemap %t/frameworks/cdecl_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/cdecl_implementation.h %t/frameworks/cdecl_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t/frameworks/cdecl_implementation.framework/cdecl_implementation) -enable-experimental-feature CImplementation -emit-module-path %t/frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name cdecl_implementation -F %t/frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t/frameworks/cdecl_implementation.framework/cdecl_implementation %S/cdecl_implementation.swift -target %target-stable-abi-triple

//
// Execute this file
//
// RUN: %empty-directory(%t/swiftmod)
// RUN: %target-build-swift %s -module-cache-path %t/swiftmod/mcp -F %t/frameworks -o %t/swiftmod/a.out -module-name main -target %target-stable-abi-triple
// RUN: %target-codesign %t/swiftmod/a.out
// RUN: %target-run %t/swiftmod/a.out | %FileCheck %s

//
// Execute again, without the swiftmodule this time
//
// RUN: mv %t/frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule %t/frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule.disabled
// RUN: %empty-directory(%t/clangmod)
// RUN: %target-build-swift %s -module-cache-path %t/clangmod/mcp -F %t/frameworks -o %t/clangmod/a.out -module-name main -target %target-stable-abi-triple
// RUN: %target-codesign %t/clangmod/a.out
// RUN: %target-run %t/clangmod/a.out | %FileCheck %s

// REQUIRES: executable_test

import cdecl_implementation

implFunc(2023 - 34)
// CHECK: implFunc(1989)
