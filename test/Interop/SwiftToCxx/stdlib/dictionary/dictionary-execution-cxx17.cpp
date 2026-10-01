// RUN: %empty-directory(%t)
// RUN: split-file %S/dictionary-execution.cpp %t

// RUN: %target-swift-frontend %t/use-dictionary.swift -module-name UseDictionary -cxx-interoperability-mode=default -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/UseDictionary.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++17 -c %t/dictionary-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-interop-build-swift %t/use-dictionary.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseDictionary -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %S/dictionary-execution.cpp

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX
