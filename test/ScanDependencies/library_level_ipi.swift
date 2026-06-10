// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Scanner reads -library-level ipi from .swiftinterface header flags.
// RUN: %target-swift-frontend -scan-dependencies %t/IPIClient.swift -o %t/deps.json -I %S/Inputs/Swift
// RUN: %validate-json %t/deps.json | %FileCheck %s --check-prefix CHECK-INTERFACE

// CHECK-INTERFACE:      "swift": "IPILib"
// CHECK-INTERFACE:      "libraryLevel": "ipi"

// RUN: %target-swift-frontend -emit-module %t/IPIBinaryLib.swift -module-name IPIBinaryLib \
// RUN:   -o %t/IPIBinaryLib.swiftmodule -library-level ipi \
// RUN:   -enable-library-evolution -emit-module-interface-path %t/IPIBinaryLib.swiftinterface
// Verify -library-level ipi is preserved in emitted .swiftinterface.
// RUN: %FileCheck %s --check-prefix CHECK-FLAGS < %t/IPIBinaryLib.swiftinterface
// CHECK-FLAGS: swift-module-flags:{{.*}}-library-level ipi

// Scanner reads library level from serialized .swiftmodule.
// RUN: rm %t/IPIBinaryLib.swiftinterface
// RUN: %target-swift-frontend -scan-dependencies %t/IPIBinaryClient.swift -o %t/bin_deps.json -I %t
// RUN: %validate-json %t/bin_deps.json | %FileCheck %s --check-prefix CHECK-BINARY

// CHECK-BINARY:      "swiftPrebuiltExternal": "IPIBinaryLib"
// CHECK-BINARY:      "libraryLevel": "ipi"

//--- IPIClient.swift
import IPILib

//--- IPIBinaryLib.swift
public func ipiBinaryFunc() {}

//--- IPIBinaryClient.swift
import IPIBinaryLib
