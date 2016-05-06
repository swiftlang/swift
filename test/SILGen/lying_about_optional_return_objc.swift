// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/block_property_in_objc_class.h -emit-silgen %s | FileCheck %s
// REQUIRES: objc_interop

// CHECK-LABEL: sil hidden @_TF32lying_about_optional_return_objc45optionalChainingForeignFunctionTypeProperties
func optionalChainingForeignFunctionTypeProperties(b: BlockProperty?) {
  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  b?.readWriteBlock()

  // CHECK: unchecked_bitwise_cast
  _ = b?.readWriteBlock

  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  b?.readOnlyBlock()

  // CHECK: unchecked_bitwise_cast
  _ = b?.readOnlyBlock

  // CHECK: unchecked_trivial_bit_cast
  _ = b?.selector

  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  _ = b?.voidReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.voidPointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.opaquePointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.pointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.constPointerReturning()
  // CHECK: unchecked_ref_cast
  _ = b?.idReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.selectorReturning()
  // CHECK: unchecked_ref_cast
  _ = b?.objectReturning()
  // CHECK: unchecked_ref_cast
  _ = b?[b!]

  let dynamic: AnyObject? = b!

  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  _ = dynamic?.voidReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.voidPointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.opaquePointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.pointerReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.constPointerReturning()
  // CHECK: unchecked_ref_cast
  _ = dynamic?.idReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selectorReturning()
  // CHECK: unchecked_ref_cast
  _ = dynamic?.objectReturning()
  // CHECK: unchecked_ref_cast
  _ = dynamic?[b]
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selector

  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  _ = dynamic?.voidReturning?()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.voidPointerReturning?()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.opaquePointerReturning?()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.pointerReturning?()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.constPointerReturning?()
  // CHECK: unchecked_ref_cast
  _ = dynamic?.idReturning?()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selectorReturning?()
  // CHECK: unchecked_ref_cast
  _ = dynamic?.idReturning?()
  _ = dynamic?.objectReturning?()
}
