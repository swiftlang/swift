// RUN: %empty-directory(%t)
// RUN: echo '@available(macOS 12.0, iOS 15.0, macCatalyst 15.0, *)' > %t/Foo.swift
// RUN: echo 'public struct Foo {}' >> %t/Foo.swift

// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/Foo.swiftinterface -enable-library-evolution %t/Foo.swift
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// CHECK: macCatalyst
