// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -O -emit-module -o %t/MyCustomMessage.swiftmodule %S/Inputs/MyCustomMessage2.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -O -c -I %t %s -enable-experimental-feature Embedded -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

import MyCustomMessage

xprint("xprint \(42) \("my string")")
// CHECK: xprint %{{(ll)?}}d %s
// CHECK: 42
// CHECK: my string
