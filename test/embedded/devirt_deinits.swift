// Embedded version of test/SILOptimizer/devirt_deinits.swift

// RUN: %target-swift-frontend -O -c -module-name=test -I %t %S/../SILOptimizer/devirt_deinits.swift -enable-experimental-feature Embedded -parse-as-library -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck -check-prefix CHECK-OUTPUT %S/../SILOptimizer/devirt_deinits.swift

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
