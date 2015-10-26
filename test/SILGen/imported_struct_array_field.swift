// RUN: %target-swift-frontend -emit-silgen -import-objc-header %S/Inputs/array_typedef.h %s | FileCheck %s

// CHECK-LABEL: sil shared [transparent] @_TFVSC4NameC{{.*}} : $@convention(thin) (UInt8, UInt8, UInt8, UInt8, @thin Name.Type) -> Name
func useImportedArrayTypedefInit() -> Name {
  return Name(name: (0, 0, 0, 0))
}
