// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name nestedprotocolsource -emit-module-path %t/nestedprotocolsource.swiftmodule -primary-file %S/Inputs/nested-protocols.swift
// RUN: %target-swift-frontend -emit-sil %s -I %t | %FileCheck %s
import nestedprotocolsource

// CHECK: usesNested<A>(_:)
// CHECK-NEXT: Isolation: unspecified
// CHECK-NEXT: sil @$s4main10usesNestedyyx20nestedprotocolsource5OuterV5InnerRzlF :
public func usesNested(_ x: some Outer.Inner) {}

// CHECK: usesNestedInExtension<A>(_:)
// CHECK-NEXT: Isolation: unspecified
// CHECK-NEXT: sil @$s4main21usesNestedInExtensionyyx20nestedprotocolsource5OuterV05InnerdE0RzlF :
public func usesNestedInExtension(_ x: some Outer.InnerInExtension) {}
