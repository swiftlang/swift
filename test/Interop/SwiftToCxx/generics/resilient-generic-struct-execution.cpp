// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -enable-library-evolution -module-name Generics -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-struct-in-cxx.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -enable-library-evolution -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %S/generic-struct-execution.cpp

// REQUIRES: executable_test

#include "generic-struct-execution.cpp"
