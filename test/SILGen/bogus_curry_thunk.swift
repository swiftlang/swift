// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class Container {
  class NestedType {}

  func someFunc1() {
    // This constructor call should not require a curry thunk.
    _ = Container.NestedType()
  }

  func someFunc2() {
    // This constructor call should not require a curry thunk.
    _ = Self.NestedType()
  }

  func someFunc3() {
    let m = Container.self

    // This constructor call should not require a curry thunk.
    _ = m.NestedType()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s17bogus_curry_thunk9ContainerC9someFunc1yyF : $@convention(method) (@guaranteed Container) -> () {
// CHECK: bb0(%0 : @guaranteed $Container):
// CHECK:   [[META2:%.*]] = metatype $@thick Container.NestedType.Type
// CHECK:   [[FN:%.*]] = function_ref @$s17bogus_curry_thunk9ContainerC10NestedTypeCAEycfC : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   [[RESULT:%.*]] = apply [[FN]]([[META2]]) : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   ignored_use [[RESULT]]
// CHECK:   destroy_value [[RESULT]]
// CHECK:   [[TUPLE:%.*]] = tuple ()
// CHECK:   return [[TUPLE]]
// CHECK: }


// CHECK-LABEL: sil hidden [ossa] @$s17bogus_curry_thunk9ContainerC9someFunc2yyF : $@convention(method) (@guaranteed Container) -> () {
// CHECK: bb0(%0 : @guaranteed $Container):
// CHECK:   [[META2:%.*]] = metatype $@thick Container.NestedType.Type
// CHECK:   [[FN:%.*]] = function_ref @$s17bogus_curry_thunk9ContainerC10NestedTypeCAEycfC : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   [[RESULT:%.*]] = apply [[FN]]([[META2]]) : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   ignored_use [[RESULT]]
// CHECK:   destroy_value [[RESULT]]
// CHECK:   [[TUPLE:%.*]] = tuple ()
// CHECK:   return [[TUPLE]]
// CHECK: }


// CHECK-LABEL: sil hidden [ossa] @$s17bogus_curry_thunk9ContainerC9someFunc3yyF : $@convention(method) (@guaranteed Container) -> () {
// CHECK: bb0(%0 : @guaranteed $Container):
// CHECK:   [[META:%.*]] = metatype $@thick Container.Type
// CHECK:   [[META2:%.*]] = metatype $@thick Container.NestedType.Type
// CHECK:   [[FN:%.*]] = function_ref @$s17bogus_curry_thunk9ContainerC10NestedTypeCAEycfC : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   [[RESULT:%.*]] = apply [[FN]]([[META2]]) : $@convention(method) (@thick Container.NestedType.Type) -> @owned Container.NestedType
// CHECK:   ignored_use [[RESULT]]
// CHECK:   destroy_value [[RESULT]]
// CHECK:   [[TUPLE:%.*]] = tuple ()
// CHECK:   return [[TUPLE]]
// CHECK: }
