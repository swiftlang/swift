// RUN: %target-swift-emit-silgen -swift-version 5 %s | %FileCheck %s --check-prefix=CHECK --check-prefix=FRAGILE
// RUN: %target-swift-emit-silgen -swift-version 5 -enable-library-evolution %s | %FileCheck %s --check-prefix=CHECK --check-prefix=RESILIENT

// RUN: %target-swift-frontend -swift-version 5 -emit-ir %s
// RUN: %target-swift-frontend -swift-version 5 -emit-ir -enable-library-evolution %s

// Swift 4 mode tests pass -sil-verify-none because the SIL verifier trips on
// the reference to an inaccessible setter.
// RUN: %target-swift-emit-silgen -swift-version 4 -sil-verify-none %s | %FileCheck %s --check-prefix=SWIFT4 --check-prefix=SWIFT4-FRAGILE
// RUN: %target-swift-emit-silgen -swift-version 4 -sil-verify-none -enable-library-evolution %s | %FileCheck %s --check-prefix=SWIFT4 --check-prefix=SWIFT4-RESILIENT

public struct PrivateSetterStruct {
  public private(set) var computed: Int { get { return 0 } set { } }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s{{.*}}usesKeypathWithPrivateSetteryyF : $@convention(thin) () -> ()
// SWIFT4-LABEL: sil [serialized] [ossa] @$s{{.*}}usesKeypathWithPrivateSetteryyF : $@convention(thin) () -> ()
@inlinable public func usesKeypathWithPrivateSetter() {
  // FRAGILE: keypath $KeyPath<PrivateSetterStruct, Int>, (root $PrivateSetterStruct; gettable_property $Int,  id @{{.*}}, getter @{{.*}} : $@convention(keypath_accessor_getter) (@in_guaranteed PrivateSetterStruct) -> @out Int)
  // RESILIENT: keypath $KeyPath<PrivateSetterStruct, Int>, (root $PrivateSetterStruct; gettable_property $Int,  id @{{.*}}, getter @{{.*}} : $@convention(keypath_accessor_getter) (@in_guaranteed PrivateSetterStruct) -> @out Int, external #PrivateSetterStruct.computed)
  // SWIFT4-FRAGILE: keypath $WritableKeyPath<PrivateSetterStruct, Int>, (root $PrivateSetterStruct; settable_property $Int,  id @{{.*}}, getter @{{.*}} : $@convention(keypath_accessor_getter) (@in_guaranteed PrivateSetterStruct) -> @out Int, setter @{{.*}} : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @inout PrivateSetterStruct) -> ())
  // SWIFT4-RESILIENT: keypath $WritableKeyPath<PrivateSetterStruct, Int>, (root $PrivateSetterStruct; gettable_property $Int,  id @{{.*}}, getter @{{.*}} : $@convention(keypath_accessor_getter) (@in_guaranteed PrivateSetterStruct) -> @out Int, external #PrivateSetterStruct.computed)
  _ = \PrivateSetterStruct.computed
}
