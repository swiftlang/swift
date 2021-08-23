// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module %t/SecretLib.swift -o %t/SecretLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Client.swift -I %t -emit-module-interface-path %t/Client.swiftinterface -enable-library-evolution -swift-version 5

/// The indirect conformance of `s` to `_p` should not be printed. (rdar://78718838)
// RUN: cat %t/Client.swiftinterface | %FileCheck %s
// CHECK-NOT: SecretLib

// BEGIN SecretLib.swift
public protocol _p {}
public struct _s : _p {}

// BEGIN Client.swift
@_implementationOnly import SecretLib
protocol p : _p {}
public struct s : p {}
public func test(s1 : s) {}
