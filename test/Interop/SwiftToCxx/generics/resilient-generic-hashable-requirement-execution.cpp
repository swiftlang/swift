// RUN: %empty-directory(%t)
// RUN: split-file %S/generic-hashable-requirement-execution.cpp %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -enable-library-evolution -cxx-interoperability-mode=default -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/generic-hashable-execution.cpp -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %t/generics.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -enable-library-evolution -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX
// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %S/generic-hashable-requirement-execution.cpp

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX
