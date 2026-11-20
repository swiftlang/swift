// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -primary-file %s %S/Inputs/property_wrappers_multifile_other.swift
// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/property_wrappers_multifile_other.swift | %FileCheck %s

// Make sure we can handle kicking the discriminator request in a seconday for the property
// wrapper backing init.
// CHECK-LABEL: sil [ossa] @$s27property_wrappers_multifile21callsWrappedParameteryyF : $@convention(thin) () -> () {
// CHECK: function_ref @$s27property_wrappers_multifile21takesWrappedParameter5valueyAA0F7WrapperVySiG_tFACL_SivpfP : $@convention(thin) (Int) -> ParameterWrapper<Int>
// CHECK: } // end sil function '$s27property_wrappers_multifile21callsWrappedParameteryyF'
public func callsWrappedParameter() {
  takesWrappedParameter(value: 0)
}

public class YourClass : MyClass {}

// CHECK-LABEL: sil_vtable [serialized] YourClass {
// CHECK-NEXT:    #MyClass.init!allocator: (MyClass.Type) -> () -> MyClass : @$s27property_wrappers_multifile9YourClassCACycfC [override]
// CHECK-NEXT:    #MyClass.instanceProperty!getter: (MyClass) -> () -> Bool : @$s27property_wrappers_multifile7MyClassC16instancePropertySbvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!getter: (MyClass) -> () -> PropertyWrapper : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvg [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!setter: (MyClass) -> (PropertyWrapper) -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvs [inherited]
// CHECK-NEXT:    #MyClass.$instanceProperty!modify: (MyClass) -> @yield_once () yields (inout PropertyWrapper) -> () : @$s27property_wrappers_multifile7MyClassC17$instancePropertyAA0G7WrapperVvM [inherited]
// CHECK-NEXT:    #YourClass.deinit!deallocator: @$s27property_wrappers_multifile9YourClassCfD
// CHECK-NEXT:  }
