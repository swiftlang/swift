// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-future-triple -cxx-interoperability-mode=default -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -emit-ir -import-objc-header %S/Inputs/layout_string_nested_tail_padding.h %s | %FileCheck %s

// REQUIRES: PTRSIZE=64
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// A non-trivially-destroyable aggregate must account for its trailing padding
// so that a managed field placed after it in an enclosing aggregate lands at
// the right offset. `NestedPadded` is {block, i8} => size 16, so its layout
// string must skip 8 (one block + 7 pad), not 1. `HasBlockAfterPadding` then
// places its second block at offset 16 (op word 648518346341351432 == 0x09..08),
// not offset 9 (648518346341351425 == 0x09..01) which would release a block
// from a mid-pointer address.

// CHECK-DAG: @"type_layout_string So12NestedPaddedV" = internal constant <{ i64, i64, i64, i64 }> <{ i64 0, i64 8, i64 648518346341351424, i64 8 }>
// CHECK-DAG: @"type_layout_string So20HasBlockAfterPaddingV" = internal constant <{ i64, i64, i64, i64, i64 }> <{ i64 0, i64 16, i64 648518346341351424, i64 648518346341351432, i64 0 }>

// Embedding the imported types in Swift structs forces their layout strings to
// be emitted in this module.
public struct WrapNested { public var v: NestedPadded }
public struct WrapOuter { public var v: HasBlockAfterPadding }
