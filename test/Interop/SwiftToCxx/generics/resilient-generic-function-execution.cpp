// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-function-in-cxx.swift -module-name Functions -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/generic-function-in-cxx.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -enable-library-evolution -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution | %FileCheck %S/generic-function-execution.cpp

// REQUIRES: executable_test

#include "generic-function-execution.cpp"
