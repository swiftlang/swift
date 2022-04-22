// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module-path %t/Other.swiftmodule -module-name Other %S/Inputs/other.swift
// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module-path /dev/null -emit-module-interface-path %t/ExportedImport.swiftmodule -module-name ExportedImport %s -I %t

// RUN: %FileCheck --input-file %t/ExportedImport.swiftmodule %s

// CHECK-NOT: otherFileFunction

@_exported import Other

// CHECK: public struct SomeStruct
public struct SomeStruct {}
