// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Importing the Clang module alone should not force the witness tables of
// the imported types' conformances.

// CHECK-NOT: sil_witness_table shared NSRuncingOptions: RawRepresentable module gizmo
// CHECK-NOT: sil_witness_table shared NSRuncingOptions: Equatable module gizmo
// CHECK-NOT: sil_witness_table shared NSRuncingOptions: Hashable module gizmo
// CHECK-NOT: sil_witness_table shared NSFungingMask: RawRepresentable module gizmo
// CHECK-NOT: sil_witness_table hidden NSRuncingOptions: Bub module objc_enum

