
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/type_layout_dumper_other.swiftmodule -module-name=type_layout_dumper_other %S/Inputs/type_layout_dumper_other.swift

// RUN: %target-swift-frontend -dump-type-info -type-info-dump-filter=fragile -I %t %s | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx

import type_layout_dumper_other

// CHECK:      ---
// CHECK-NEXT: Name:            type_layout_dumper_other
// CHECK-NEXT: Decls:
// CHECK-NEXT:   - Name:            24type_layout_dumper_other21ConcreteFragileStructV
// CHECK-NEXT:     Size:            4
// CHECK-NEXT:     Alignment:       4
// CHECK-NEXT:     ExtraInhabitants: 0
// CHECK-NEXT:   - Name:            24type_layout_dumper_other25NonDependentFragileStructV
// CHECK-NEXT:     Size:            8
// CHECK-NEXT:     Alignment:       8
// CHECK-NEXT:     ExtraInhabitants: 2147483647
// CHECK-NEXT: ...

// NEGATIVE-NOT: Name: SomeClass
// NEGATIVE-NOT: Name: SomeProtocol
// NEGATIVE-NOT: Name: ConcreteFragileStruct.NestedResilientStruct{{$}}
// NEGATIVE-NOT: Name: ConcreteResilientEnum{{$}}
// NEGATIVE-NOT: Name: ConcreteResilientStruct{{$}}
// NEGATIVE-NOT: Name: DependentFragileStruct{{$}}
// NEGATIVE-NOT: Name: DependentResilientEnum{{$}}
// NEGATIVE-NOT: Name: DependentResilientEnum.NestedNonDependentResilientEnum{{$}}
// NEGATIVE-NOT: Name: DependentResilientStruct{{$}}
// NEGATIVE-NOT: Name: DependentResilientStruct.NestedNonDependentResilientStruct{{$}}
// NEGATIVE-NOT: Name: Int.NestedInExtension{{$}}
// NEGATIVE-NOT: Name: NonDependentResilientEnum{{$}}
// NEGATIVE-NOT: Name: NonDependentResilientStruct{{$}}
