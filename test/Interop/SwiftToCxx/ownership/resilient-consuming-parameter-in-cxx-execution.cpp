// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h -enable-library-evolution

// RUN: %target-interop-build-clangxx -c %S/consuming-parameter-in-cxx-execution.cpp -I %t -o %t/swift-consume-execution.o
// RUN: %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-consume-execution-evo -Xlinker %t/swift-consume-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution

// RUN: %target-codesign %t/swift-consume-execution-evo
// RUN: %target-run %t/swift-consume-execution-evo | %FileCheck %S/consuming-parameter-in-cxx-execution.cpp

// RUN: %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h -enable-library-evolution -enable-experimental-feature GenerateConsumingValueParametersInCXX
// RUN: %target-interop-build-clangxx -std=c++17 -DTEST_CONSUMING_MOVES -c %S/consuming-parameter-in-cxx-execution.cpp -I %t -o %t/swift-move-execution.o
// RUN: %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-move-execution-evo -Xlinker %t/swift-move-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution
// RUN: %target-codesign %t/swift-move-execution-evo
// RUN: %target-run %t/swift-move-execution-evo | %FileCheck %S/consuming-parameter-in-cxx-execution.cpp

// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-swift-frontend %S/consuming-parameter-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/consuming.h -enable-library-evolution -enable-experimental-feature GenerateConsumingValueParametersInCXX -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-interop-build-clangxx -std=c++17 -DTEST_CONSUMING_MOVES -DTEST_THROWING_HANDOFF -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -c %S/consuming-parameter-in-cxx-execution.cpp -I %t -o %t/swift-throwing-execution.o %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-throwing-execution -Xlinker %t/swift-throwing-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-codesign %t/swift-throwing-execution %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-run %t/swift-throwing-execution | %FileCheck %S/consuming-parameter-in-cxx-execution.cpp %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-interop-build-clangxx -std=c++17 -fno-exceptions -DTEST_CONSUMING_MOVES -DTEST_THROWING_HANDOFF -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -c %S/consuming-parameter-in-cxx-execution.cpp -I %t -o %t/swift-expected-execution.o %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-interop-build-swift %S/consuming-parameter-in-cxx.swift -o %t/swift-expected-execution -Xlinker %t/swift-expected-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-library-evolution %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-codesign %t/swift-expected-execution %}
// RUN: %if swift_feature_GenerateBindingsForThrowingFunctionsInCXX %{ %target-run %t/swift-expected-execution | %FileCheck %S/consuming-parameter-in-cxx-execution.cpp %}

// REQUIRES: executable_test
// REQUIRES: swift_feature_GenerateConsumingValueParametersInCXX
