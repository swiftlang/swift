// RUN: %target-swift-frontend -emit-module %s -enable-library-evolution -swift-version 5 -module-name Library -emit-module-interface-path %t/Library.swiftinterface -o %t/Library.swiftmodule

// RUN: echo "import Library" > %t/Client.swift
// RUN: %target-swift-frontend -emit-module %t/Client.swift -module-name Client -o %t/Client.swiftmodule -I %t

// RUN: rm %t/Library.swiftmodule
// RUN: %target-sil-opt -enable-sil-verify-all %t/Client.swiftmodule -module-name Client

@available(*, unavailable)
public struct Unavailable {}

@available(*, unavailable)
public func usesUnavailable(_ u: Unavailable) {}
