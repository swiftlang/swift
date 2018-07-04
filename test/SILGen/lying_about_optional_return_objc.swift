// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/block_property_in_objc_class.h -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$S32lying_about_optional_return_objc0C37ChainingForeignFunctionTypeProperties{{[_0-9a-zA-Z]*}}F
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
  // CHECK: unchecked_trivial_bit_cast {{.*}} $UnsafeMutableRawPointer to $Optional
  _ = dynamic?.voidPointerReturning()
  // CHECK: unchecked_trivial_bit_cast {{.*}} $OpaquePointer to $Optional
  _ = dynamic?.opaquePointerReturning()
  // CHECK: unchecked_trivial_bit_cast {{.*}} $UnsafeMutablePointer{{.*}} to $Optional
  _ = dynamic?.pointerReturning()
  // CHECK: unchecked_trivial_bit_cast {{.*}} $UnsafePointer{{.*}} to $Optional
  _ = dynamic?.constPointerReturning()
  // CHECK: unchecked_trivial_bit_cast {{.*}} $Selector to $Optional
  _ = dynamic?.selectorReturning()
  // CHECK: unchecked_ref_cast {{.*}} $BlockProperty to $Optional
  _ = dynamic?.objectReturning()
  // FIXME: Doesn't opaquely cast the selector result!
  // C/HECK: unchecked_trivial_bit_cast {{.*}} $Selector to $Optional
  _ = dynamic?.selector

  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> ()>, #Optional.some
  _ = dynamic?.voidReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> UnsafeMutableRawPointer>, #Optional.some
  _ = dynamic?.voidPointerReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> OpaquePointer>, #Optional.some
  _ = dynamic?.opaquePointerReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> UnsafeMutablePointer{{.*}}>, #Optional.some
  _ = dynamic?.pointerReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> UnsafePointer{{.*}}>, #Optional.some
  _ = dynamic?.constPointerReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> Selector>, #Optional.some
  _ = dynamic?.selectorReturning
  // CHECK: inject_enum_addr {{%.*}} : $*Optional<{{.*}} -> @owned BlockProperty>, #Optional.some
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
