
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/type_layout_dumper_other.swiftmodule -module-name=type_layout_dumper_other %S/Inputs/type_layout_dumper_other.swift

// RUN: %target-swift-frontend -dump-type-info -type-info-dump-filter=resilient -I %t %s | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx

import type_layout_dumper_other

// CHECK:      ---
// CHECK-NEXT: Name:            type_layout_dumper_other
// CHECK-NEXT: Decls:
// CHECK-NEXT:   - Name:            24type_layout_dumper_other21ConcreteFragileStructV015NestedResilientG0V
// CHECK-NEXT:     Size:            0
// CHECK-NEXT:     Alignment:       1
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other21ConcreteResilientEnumO
// CHECK-NEXT:     Size:            9
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other22DependentResilientEnumO09NestedNonefG0O
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other23ConcreteResilientStructV
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other24DependentResilientStructV09NestedNonefG0V
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 1
// CHECK-NEXT:   - Name:            24type_layout_dumper_other25NonDependentResilientEnumO
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other27NonDependentResilientStructV
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 2147483647
// CHECK-NEXT:   - Name:            Si24type_layout_dumper_otherE17NestedInExtensionV
// CHECK-NEXT:     Size:            4
// CHECK-NEXT:     Alignment:       4
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT: ...
