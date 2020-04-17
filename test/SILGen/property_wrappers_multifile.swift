// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/property_wrappers_multifile_other.swift | %FileCheck %s

public class YourClass : MyClass {}

// CHECK-LABEL: sil_vtable [serialized] YourClass {
// CHECK-NEXT:    #MyClass.init!allocator.1: (MyClass.Type) -> () -> MyClass : @$s27property_wrappers_multifile9YourClassCACycfC [override]
// CHECK-NEXT:    #MyClass.instanceProperty!getter.1: (MyClass) -> () -> Bool : @$s27property_wrappers_multifile7MyClassC16instancePropertySbvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!getter.1: (MyClass) -> () -> PropertyWrapper : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!setter.1: (MyClass) -> (PropertyWrapper) -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvs [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!modify.1: (MyClass) -> () -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvM [inherited]
// CHECK-NEXT:    #YourClass.deinit!deallocator.1: @$s27property_wrappers_multifile9YourClassCfD
// CHECK-NEXT:  }
