// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/property_wrappers_multifile_other.swift | %FileCheck %s

public class YourClass : MyClass {}

// CHECK-LABEL: sil_vtable [serialized] YourClass {
// CHECK-NEXT:    #MyClass.init!allocator: (MyClass.Type) -> () -> MyClass : @$s27property_wrappers_multifile9YourClassCACycfC [override]
// CHECK-NEXT:    #MyClass.instanceProperty!getter: (MyClass) -> () -> Bool : @$s27property_wrappers_multifile7MyClassC16instancePropertySbvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!getter: (MyClass) -> () -> PropertyWrapper : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!setter: (MyClass) -> (PropertyWrapper) -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvs [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!modify: (MyClass) -> () -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvM [inherited]
// CHECK-NEXT:    #YourClass.deinit!deallocator: @$s27property_wrappers_multifile9YourClassCfD
// CHECK-NEXT:  }