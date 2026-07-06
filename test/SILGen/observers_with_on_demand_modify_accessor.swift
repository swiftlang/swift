// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
// REQUIRES: objc_interop

public protocol Proto {
  var property: Int { get set }
}

public class ClassWithDynamicObservedProperty : Proto {
  @objc public dynamic var property: Int = 0 {
    didSet {}
  }
}

// CHECK-LABEL: sil shared [serialized] [ossa] @$s40observers_with_on_demand_modify_accessor32ClassWithDynamicObservedPropertyC8propertySivM : $@yield_once @convention(method) (@guaranteed ClassWithDynamicObservedProperty) -> @yields @inout Int {
// CHECK: objc_method %0 : $ClassWithDynamicObservedProperty, #ClassWithDynamicObservedProperty.property!getter.foreign
// CHECK: objc_method %0 : $ClassWithDynamicObservedProperty, #ClassWithDynamicObservedProperty.property!setter.foreign
// CHECK: return
