// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-class-virtual-method-dispatch.swift -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h -enable-library-evolution

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o
// RUN: %target-interop-build-swift %S/swift-class-virtual-method-dispatch.swift -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -module-name Class -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution | %FileCheck %S/swift-class-virtual-method-dispatch-execution.cpp

// REQUIRES: executable_test

#include "swift-class-virtual-method-dispatch-execution.cpp"
