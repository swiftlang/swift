// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s --check-prefix=FRAGILE --check-prefix=CHECK
// RUN: %target-swift-emit-silgen -enable-library-evolution -parse-as-library %s | %FileCheck %s --check-prefix=RESILIENT --check-prefix=CHECK

// RUN: %target-swift-emit-silgen -parse-as-library -enable-testing %s
// RUN: %target-swift-emit-silgen -parse-as-library -enable-testing -enable-library-evolution %s

public let global = 0

struct InternalStruct {
  var storedProperty = global
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s22fixed_layout_attribute14InternalStructV14storedPropertySivpfi : $@convention(thin) () -> Int
//
//    ... okay to directly reference the addressor here:
// CHECK: function_ref @$s22fixed_layout_attribute6globalSivau
// CHECK: return

public struct NonFixedStruct {
  public var storedProperty = global
}

// FRAGILE-LABEL: sil [transparent] [ossa] @$s22fixed_layout_attribute14NonFixedStructV14storedPropertySivpfi : $@convention(thin) () -> Int
// RESILIENT-LABEL: sil hidden [transparent] [ossa] @$s22fixed_layout_attribute14NonFixedStructV14storedPropertySivpfi : $@convention(thin) () -> Int
//
//    ... okay to directly reference the addressor here:
// CHECK: function_ref @$s22fixed_layout_attribute6globalSivau
// CHECK: return

@frozen
public struct FixedStruct {
  public var storedProperty = global
}

// CHECK-LABEL: sil non_abi [transparent] [serialized] [ossa] @$s22fixed_layout_attribute11FixedStructV14storedPropertySivpfi : $@convention(thin) () -> Int
//
//    ... a fragile build can still reference the addressor:
// FRAGILE: function_ref @$s22fixed_layout_attribute6globalSivau

//    ... a resilient build has to use the getter because the addressor
//    is not public, and the initializer is serialized:
// RESILIENT: function_ref @$s22fixed_layout_attribute6globalSivg

// CHECK: return

// This would crash with -enable-testing
private let privateGlobal = 0

struct AnotherInternalStruct {
  var storedProperty = privateGlobal
}

// Static properties in fixed-layout type is still resilient

@frozen
public struct HasStaticProperty {
  public static var staticProperty: Int = 0
}

// CHECK-LABEL: sil [ossa] @$s22fixed_layout_attribute18usesStaticPropertyyyF : $@convention(thin) () -> ()
// CHECK: function_ref @$s22fixed_layout_attribute17HasStaticPropertyV06staticF0Sivau : $@convention(thin) () -> Builtin.RawPointer
// CHECK: return
public func usesStaticProperty() {
  _ = HasStaticProperty.staticProperty
}

// CHECK-LABEL: sil [serialized] [ossa] @$s22fixed_layout_attribute27usesStaticPropertyInlinableyyF : $@convention(thin) () -> ()

@inlinable
public func usesStaticPropertyInlinable() {
  _ = HasStaticProperty.staticProperty
}
