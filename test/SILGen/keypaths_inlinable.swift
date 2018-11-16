// RUN: %target-swift-emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=FRAGILE
// RUN: %target-swift-emit-silgen -enable-resilience %s | %FileCheck %s --check-prefix=CHECK --check-prefix=RESILIENT

public struct KeypathStruct {
  public var stored: Int = 0
  public var computed: String { get { } set { } }

  public subscript(x: Int, y: String) -> Bool { get { } set { } }
}

// CHECK-LABEL: sil [serialized] @$s18keypaths_inlinable11usesKeypathyyF : $@convention(thin) () -> ()
@inlinable public func usesKeypath() {
  // FRAGILE: keypath $WritableKeyPath<KeypathStruct, Int>, (root $KeypathStruct; stored_property #KeypathStruct.stored : $Int)
  // RESILIENT: keypath $WritableKeyPath<KeypathStruct, Int>, (root $KeypathStruct; settable_property $Int,  id ##KeypathStruct.stored, getter @$s18keypaths_inlinable13KeypathStructV6storedSivpACTKq : $@convention(thin) (@in_guaranteed KeypathStruct) -> @out Int, setter @$s18keypaths_inlinable13KeypathStructV6storedSivpACTkq : $@convention(thin) (@in_guaranteed Int, @inout KeypathStruct) -> (), external #KeypathStruct.stored)
  _ = \KeypathStruct.stored

  // FRAGILE: keypath $WritableKeyPath<KeypathStruct, String>, (root $KeypathStruct; settable_property $String,  id @$s18keypaths_inlinable13KeypathStructV8computedSSvg : $@convention(method) (KeypathStruct) -> @owned String, getter @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTKq : $@convention(thin) (@in_guaranteed KeypathStruct) -> @out String, setter @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTkq : $@convention(thin) (@in_guaranteed String, @inout KeypathStruct) -> ()
  // RESILIENT: keypath $WritableKeyPath<KeypathStruct, String>, (root $KeypathStruct; settable_property $String,  id @$s18keypaths_inlinable13KeypathStructV8computedSSvg : $@convention(method) (@in_guaranteed KeypathStruct) -> @owned String, getter @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTKq : $@convention(thin) (@in_guaranteed KeypathStruct) -> @out String, setter @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTkq : $@convention(thin) (@in_guaranteed String, @inout KeypathStruct) -> (), external #KeypathStruct.computed
  _ = \KeypathStruct.computed

  // FRAGILE: keypath $WritableKeyPath<KeypathStruct, Bool>, (root $KeypathStruct; settable_property $Bool,  id @$s18keypaths_inlinable13KeypathStructVySbSi_SStcig : $@convention(method) (Int, @guaranteed String, KeypathStruct) -> Bool, getter @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTKq : $@convention(thin) (@in_guaranteed KeypathStruct, UnsafeRawPointer) -> @out Bool, setter @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTkq : $@convention(thin) (@in_guaranteed Bool, @inout KeypathStruct, UnsafeRawPointer) -> (), indices [%$0 : $Int : $Int, %$1 : $String : $String], indices_equals @$sSiSSTHq : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSiSSThq : $@convention(thin) (UnsafeRawPointer) -> Int) ({{.*}})
  // RESILIENT: keypath $WritableKeyPath<KeypathStruct, Bool>, (root $KeypathStruct; settable_property $Bool,  id @$s18keypaths_inlinable13KeypathStructVySbSi_SStcig : $@convention(method) (Int, @guaranteed String, @in_guaranteed KeypathStruct) -> Bool, getter @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTKq : $@convention(thin) (@in_guaranteed KeypathStruct, UnsafeRawPointer) -> @out Bool, setter @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTkq : $@convention(thin) (@in_guaranteed Bool, @inout KeypathStruct, UnsafeRawPointer) -> (), indices [%$0 : $Int : $Int, %$1 : $String : $String], indices_equals @$sSiSSTHq : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool, indices_hash @$sSiSSThq : $@convention(thin) (UnsafeRawPointer) -> Int, external #KeypathStruct.subscript) ({{.*}})
  _ = \KeypathStruct[0, ""]
}

// CHECK-LABEL: sil shared [serializable] [thunk] @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTKq : $@convention(thin) (@in_guaranteed KeypathStruct) -> @out String

// CHECK-LABEL: sil shared [serializable] [thunk] @$s18keypaths_inlinable13KeypathStructV8computedSSvpACTkq : $@convention(thin) (@in_guaranteed String, @inout KeypathStruct) -> ()

// CHECK-LABEL: sil shared [serializable] [thunk] @$sSiSSTHq : $@convention(thin) (UnsafeRawPointer, UnsafeRawPointer) -> Bool

// CHECK-LABEL: sil shared [serializable] [thunk] @$sSiSSThq : $@convention(thin) (UnsafeRawPointer) -> Int

// CHECK-LABEL: sil shared [serializable] [thunk] @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTKq : $@convention(thin) (@in_guaranteed KeypathStruct, UnsafeRawPointer) -> @out Bool

// CHECK-LABEL: sil shared [serializable] [thunk] @$s18keypaths_inlinable13KeypathStructVySbSi_SStcipACTkq : $@convention(thin) (@in_guaranteed Bool, @inout KeypathStruct, UnsafeRawPointer) -> ()

