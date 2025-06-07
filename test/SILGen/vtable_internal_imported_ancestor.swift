// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module-path %t/Repro1.swiftmodule -module-name Repro1 %S/Inputs/vtable_internal_imported_ancestor/Repro1.swift
// RUN: %target-swift-frontend -enable-upcoming-feature InternalImportsByDefault -I %t -emit-silgen %S/Inputs/vtable_internal_imported_ancestor/Repro2.swift -primary-file %s | %FileCheck %s

// REQUIRES: swift_feature_InternalImportsByDefault

import Repro1

public class MostDerived: MidDerived {
}

// CHECK-NOT: vtable thunk
// CHECK-LABEL: sil_vtable{{.*}} MostDerived {
// CHECK:         #Base.init!allocator: {{.*}} @$s6Repro211MostDerivedCACycfC [override]
