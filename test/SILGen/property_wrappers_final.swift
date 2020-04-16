// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Test that the storage wrapper for a final property is itself final, and that
// its accessors do not appear in the vtable.

public class MyClass {
  public init() { }
    
  @PropertyWrapper()
  public static var staticProperty: Bool

  @PropertyWrapper()
  public final var instanceProperty: Bool

}

@propertyWrapper
public struct PropertyWrapper {
  public init() { }

  public var projectedValue: PropertyWrapper {
    get {
      return self
    }
    set {
      self = newValue
    }
  }

  public var wrappedValue: Bool {
    return false
  }
}

// CHECK-LABEL: sil [ossa] @$s23property_wrappers_final17useStorageWrapperyyAA7MyClassCF : $@convention(thin) (@guaranteed MyClass) -> () {
public func useStorageWrapper(_ c: MyClass) {
  // CHECK: function_ref @$s23property_wrappers_final7MyClassC15$staticPropertyAA0G7WrapperVvgZ
  _ = MyClass.$staticProperty

  // CHECK: function_ref @$s23property_wrappers_final7MyClassC15$staticPropertyAA0G7WrapperVvsZ
  MyClass.$staticProperty = PropertyWrapper()

  // CHECK: $s23property_wrappers_final7MyClassC17$instancePropertyAA0G7WrapperVvg
  _ = c.$instanceProperty

  // CHECK: $s23property_wrappers_final7MyClassC17$instancePropertyAA0G7WrapperVvs
  c.$instanceProperty = PropertyWrapper()

  // CHECK: return
}

// CHECK-LABEL: sil_vtable [serialized] MyClass {
// CHECK-NEXT:    #MyClass.init!allocator: (MyClass.Type) -> () -> MyClass : @$s23property_wrappers_final7MyClassCACycfC
// CHECK-NEXT:    #MyClass.deinit!deallocator: @$s23property_wrappers_final7MyClassCfD
// CHECK-NEXT:  }
