// RUN: %empty-directory(%t)
// RUN: split-file %S/array-execution.cpp %t

// RUN: %target-swift-frontend %t/use-array.swift -module-name UseArray -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/UseArray.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++17 -c %t/array-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/use-array.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseArray -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %S/array-execution.cpp

// REQUIRES: executable_test
