// REQUIRES: OS=linux-gnu
// REQUIRES: executable_test
// REQUIRES: swift_tools_extra

// Test that concatenated .swift_ast sections of various sizes can be parsed.

// RUN: %empty-directory(%t)

// RUN: echo "public let a0 = 0"  >%t/a0.swift

// RUN: echo "public let a1 = 0"  >%t/a1.swift
// RUN: echo "public let b1 = 0" >>%t/a1.swift

// RUN: echo "public let a2 = 0"  >%t/a2.swift
// RUN: echo "public let b2 = 0" >>%t/a2.swift
// RUN: echo "public let c2 = 0" >>%t/a2.swift

// RUN: echo "public let a3 = 0"  >%t/a3.swift
// RUN: echo "public let b3 = 0" >>%t/a3.swift
// RUN: echo "public let c3 = 0" >>%t/a3.swift
// RUN: echo "public let d3 = 0" >>%t/a3.swift

// RUN: %target-build-swift %t/a0.swift -c -g -o %t/a0.o -parse-as-library
// RUN: %target-build-swift %t/a1.swift -c -g -o %t/a1.o -parse-as-library
// RUN: %target-build-swift %t/a2.swift -c -g -o %t/a2.o -parse-as-library
// RUN: %target-build-swift %t/a3.swift -c -g -o %t/a3.o -parse-as-library

// RUN: %target-build-swift %t/a0.swift -emit-module -emit-module-path %t/a0.swiftmodule
// RUN: %target-build-swift %t/a1.swift -emit-module -emit-module-path %t/a1.swiftmodule
// RUN: %target-build-swift %t/a2.swift -emit-module -emit-module-path %t/a2.swiftmodule
// RUN: %target-build-swift %t/a3.swift -emit-module -emit-module-path %t/a3.swiftmodule

// RUN: %target-swift-modulewrap %t/a0.swiftmodule -o %t/a0-mod.o
// RUN: %target-swift-modulewrap %t/a1.swiftmodule -o %t/a1-mod.o
// RUN: %target-swift-modulewrap %t/a2.swiftmodule -o %t/a2-mod.o
// RUN: %target-swift-modulewrap %t/a3.swiftmodule -o %t/a3-mod.o

// RUN: %target-build-swift -o %t/a.out %s \
// RUN:                     %t/a0.o %t/a0-mod.o \
// RUN:                     %t/a1.o %t/a1-mod.o \
// RUN:                     %t/a2.o %t/a2-mod.o \
// RUN:                     %t/a3.o %t/a3-mod.o

// RUN: %lldb-moduleimport-test -verbose %t/a.out | %FileCheck %s
// CHECK-DAG: Importing a0...
// CHECK-DAG: Import successful!
// CHECK-DAG: Importing a1...
// CHECK-DAG: Import successful!
// CHECK-DAG: Importing a2...
// CHECK-DAG: Import successful!
// CHECK-DAG: Importing a3...

