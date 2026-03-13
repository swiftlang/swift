// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h -enable-library-evolution

// RUN: %target-interop-build-clangxx -c %S/consuming-parameter-in-cxx-execution.cpp -I %t -o %t/swift-consume-execution.o
// RUN: %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-consume-execution-evo -Xlinker %t/swift-consume-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution

// RUN: %target-codesign %t/swift-consume-execution-evo
// RUN: %target-run %t/swift-consume-execution-evo | %FileCheck %S/consuming-parameter-in-cxx-execution.cpp

// REQUIRES: executable_test
