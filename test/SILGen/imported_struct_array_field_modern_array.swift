// RUN: %target-swift-emit-silgen -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/array_typedef.h %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple > %t.sil
// RUN: %FileCheck %s --input-file %t.sil
// REQUIRES: swift_feature_ModernImportedCArrays

// CHECK-LABEL: sil shared [transparent] [serialized]{{ \[available 26.0.0\] | }}[ossa] @$sSo4NameV{{[_0-9a-zA-Z$]*}}fC : $@convention(method) (InlineArray<4, UInt8>, @thin Name.Type) -> Name
func useImportedArrayTypedefInit() -> Name {
  return Name(name: [0, 0, 0, 0])
}
