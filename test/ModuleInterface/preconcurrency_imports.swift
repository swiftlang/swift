// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/PreconcurrencyLib.swift -module-name PreconcurrencyLib -swift-version 5 -enable-library-evolution -emit-module-path %t/PreconcurrencyLib.swiftmodule -emit-module-interface-path %t/PreconcurrencyLib.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/OtherLib.swift -module-name OtherLib -swift-version 5 -enable-library-evolution -emit-module-path %t/OtherLib.swiftmodule -emit-module-interface-path %t/OtherLib.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/ClientLib.swiftinterface) -swift-version 6 %t/ClientLib_file1.swift %t/ClientLib_file2.swift -module-name ClientLib -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -module-name ClientLib -I %t
// RUN: %FileCheck %s < %t/ClientLib.swiftinterface

// CHECK: {{^}}@preconcurrency import OtherLib
// CHECK: {{^}}@preconcurrency import PreconcurrencyLib
// CHECK: public struct Struct1 : Swift.Sendable
// CHECK: public struct Struct2

//--- PreconcurrencyLib.swift

public class C {}

//--- OtherLib.swift
// Intentionally empty

//--- ClientLib_file1.swift

@preconcurrency public import PreconcurrencyLib
public import OtherLib

public struct Struct1: Sendable {
  public var c: C
}

//--- ClientLib_file2.swift

internal import PreconcurrencyLib
@preconcurrency internal import OtherLib

public struct Struct2 {}
