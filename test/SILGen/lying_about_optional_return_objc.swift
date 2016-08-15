// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/block_property_in_objc_class.h -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

// CHECK-LABEL: sil hidden @_TF32lying_about_optional_return_objc45optionalChainingForeignFunctionTypeProperties
func optionalChainingForeignFunctionTypeProperties(b: BlockProperty?) {
  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  b?.readWriteBlock()

  // CHECK: enum $Optional
  _ = b?.readWriteBlock

  // CHECK: enum $Optional<()>, #Optional.some!enumelt.1, {{%.*}} : $()
  b?.readOnlyBlock()

  // CHECK: enum $Optional
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
  // CHECK: unchecked_trivial_bit_cast
  _ = b?.selectorReturning()
  // CHECK: unchecked_ref_cast
  _ = b?.objectReturning()

  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.voidReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.voidPointerReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.opaquePointerReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.pointerReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.constPointerReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.selectorReturning
  // CHECK: enum $Optional<{{.*}} -> {{.*}}>
  _ = b?.objectReturning

  // CHECK-LABEL: debug_value {{.*}} name "dynamic"
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
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selectorReturning()
  // CHECK: unchecked_ref_cast
  _ = dynamic?.objectReturning()
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selector

  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.voidReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.voidPointerReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.opaquePointerReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.pointerReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.constPointerReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.selectorReturning
  // CHECK: unchecked_bitwise_cast {{%.*}} : $ImplicitlyUnwrappedOptional<{{.*}} -> {{.*}}> to $Optional<{{.*}} -> {{.*}}>
  _ = dynamic?.objectReturning

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
  // CHECK: unchecked_trivial_bit_cast
  _ = dynamic?.selectorReturning?()
  _ = dynamic?.objectReturning?()


}
