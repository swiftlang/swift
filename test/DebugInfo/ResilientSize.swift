// RUN: %empty-directory(%t)
//
// Compile the external swift module.
// RUN: %target-swift-frontend -g -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/resilient_protocol.swiftmodule \
// RUN:   -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
//
// RUN: %target-swift-frontend -g -I %t -emit-ir %s  -o - | %FileCheck %s
import resilient_protocol

public struct S<T> : OtherResilientProtocol  {
  public var v : T
  public func requirement() -> Int { return 42 }
}

// Test that this type has no size (instead of an incorrect size of 0).
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s13ResilientSize1SVyxGD", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: !{{[0-9]+}})
