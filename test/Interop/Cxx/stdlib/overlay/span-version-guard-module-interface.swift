// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend \
// RUN:   -swift-version 5 \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module \
// RUN:   -module-name Cxx \
// RUN:   -o %t/Cxx.swiftmodule \
// RUN:   -emit-module-interface-path %t/Cxx.swiftinterface \
// RUN:   %S/Inputs/span_interface_guard.swift

// RUN: %FileCheck %s < %t/Cxx.swiftinterface

// Verify the interface can be compiled back.
// RUN: %target-swift-frontend \
// RUN:   -compile-module-from-interface \
// RUN:   -o %t/Cxx.swiftmodule \
// RUN:   %t/Cxx.swiftinterface

// Declarations that reference Span in the Cxx module should be guarded.
// CHECK:       #if canImport(Swift, _version: 6.2)
// CHECK-NEXT:  @available(macOS 10.14.4, iOS 12.2, watchOS 5.2, tvOS 12.2, visionOS 1.0, *)
// CHECK-NEXT:  public func takeSpan(_ s: Swift::Span<Swift::Int>) -> Swift::Int
// CHECK-NEXT:  #endif

// Non-Span declarations should NOT be guarded.
// CHECK:       public func noSpan(_ x: Swift::Int) -> Swift::Int
// CHECK-NOT:   #endif
