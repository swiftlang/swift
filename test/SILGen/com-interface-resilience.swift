// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -I %t -emit-silgen %s | %FileCheck %s --check-prefix=SIL --implicit-check-not="sil_default_witness_table IBase" --implicit-check-not="sil_default_witness_table IDerived"
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -I %t -D AST_ONLY -dump-ast %s | %FileCheck %s --check-prefix=AST
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -module-name COM -emit-silgen %S/../Inputs/COM.swift | %FileCheck %s --check-prefix=IDENTITY

// A COM interface's IID fixes its requirements even in a resilient module.
// AST: protocol{{.*}} "IBase"{{.*}} access=public non_resilient
@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func base() -> CInt
}

// AST: protocol{{.*}} "IDerived"{{.*}} access=public non_resilient
@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived() -> CInt
}

// Ordinary Swift protocols still use resilient default witness tables.
// AST: protocol{{.*}} "Native"{{.*}} access=public resilient
// SIL: sil_default_witness_table Native {
public protocol Native {
  func native() -> CInt
}

#if AST_ONLY
// The fixed interface ABI does not fix a Swift implementation's class layout.
// AST: class_decl{{.*}} "Implementation"{{.*}} access=public resilient
@com
public class Implementation: IBase {
  public func base() -> CInt { 0 }
}
#endif

// Compiler-managed identity protocols are ordinary Swift protocols.
// IDENTITY-DAG: sil_default_witness_table COMInterface {
// IDENTITY-DAG: sil_default_witness_table COMActivatable {
