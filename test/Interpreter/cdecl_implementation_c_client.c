//
// Build cdecl_implementation.framework
//
// RUN: %empty-directory(%t-frameworks)
// RUN: %empty-directory(%t-frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule)
// RUN: %empty-directory(%t-frameworks/cdecl_implementation.framework/Headers)
// RUN: cp %S/Inputs/cdecl_implementation.modulemap %t-frameworks/cdecl_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/cdecl_implementation.h %t-frameworks/cdecl_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t-frameworks/cdecl_implementation.framework/cdecl_implementation) -emit-module-path %t-frameworks/cdecl_implementation.framework/Modules/cdecl_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name cdecl_implementation -F %t-frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t-frameworks/cdecl_implementation.framework/cdecl_implementation %S/cdecl_implementation.swift -enable-experimental-feature CImplementation -target %target-stable-abi-triple

//
// Execute this file
//
// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %s -isysroot %sdk -F %t-frameworks -fmodules -o %t/cdecl_implementation_objc_client
// RUN: %target-codesign %t/cdecl_implementation_objc_client
// RUN: %target-run %t/cdecl_implementation_objc_client 2>&1 | %FileCheck %s

// REQUIRES: executable_test

// FIXME: This test fails in Swift CI simulators, but I have not been able to
//        reproduce this locally.
// REQUIRES: OS=macosx

#import <cdecl_implementation/cdecl_implementation.h>
#import <stdlib.h>
#import <stdio.h>

int main() {
  implFunc(2023 - 34);
  // CHECK: implFunc(1989)

  fflush(stdout);
  return 0;
}
