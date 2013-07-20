// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

protocol Fooable {
  func foo<T>(x:T)
}

class FooClass : Fooable {
  func foo<U>(x:U) {}
}
// CHECK: define internal void @_Tnk_C17generic_witnesses8FooClass3foofS0_U__FT1xQ__T_(%swift.opaque*, %swift.opaque*, %swift.type* %T, %swift.type* %This) {
// CHECK:   [[THIS_ADDR:%.*]] = bitcast %swift.opaque* %1 to %C17generic_witnesses8FooClass**
// CHECK:   [[THIS:%.*]] = load %C17generic_witnesses8FooClass** [[THIS_ADDR]], align 8
// CHECK:   [[THIS_REFCOUNTED:%.*]] = bitcast %C17generic_witnesses8FooClass* [[THIS]] to %swift.refcounted*
// CHECK:   call void @swift_retain_noresult(%swift.refcounted* [[THIS_REFCOUNTED]])
// CHECK:   call void @_TC17generic_witnesses8FooClass3foofS0_U__FT1xQ__T_(%swift.opaque* %0, %C17generic_witnesses8FooClass* [[THIS]], %swift.type* %T)

struct FooStruct : Fooable {
  func foo<V>(x:V) {}
}
// No abstraction difference to protocol witness
// CHECK-NOT: define internal void @_Tnk_V17generic_witnesses9FooStruct3foofS0_U__FT1xQ__T_

// Force witness tables to be emitted.
var cfoo : Fooable = FooClass()
var sfoo : Fooable = FooStruct()

protocol Barrable {
  func bar<T>(x:This, y:T)
}

class BarClass : Barrable {
  func bar<U>(x:BarClass, y:U) { }
}
// CHECK: define internal void @_Tnk_C17generic_witnesses8BarClass3barfS0_U__FT1xS0_1yQ__T_(%swift.opaque*, %swift.opaque*, %swift.opaque*, %swift.type* %T, %swift.type* %This) {
// CHECK:   [[THIS_ADDR:%.*]] = bitcast %swift.opaque* %2 to %C17generic_witnesses8BarClass**
// CHECK:   [[THIS:%.*]] = load %C17generic_witnesses8BarClass** [[THIS_ADDR]], align 8
// CHECK:   [[THIS_REFCOUNTED:%.*]] = bitcast %C17generic_witnesses8BarClass* [[THIS]] to %swift.refcounted*
// CHECK:   call void @swift_retain_noresult(%swift.refcounted* [[THIS_REFCOUNTED]])
// CHECK:   [[X_ADDR:%.*]] = bitcast %swift.opaque* %0 to %C17generic_witnesses8BarClass**
// CHECK:   [[X:%.*]] = load %C17generic_witnesses8BarClass** [[X_ADDR]], align 8
// CHECK:   call void @_TC17generic_witnesses8BarClass3barfS0_U__FT1xS0_1yQ__T_(%C17generic_witnesses8BarClass* [[X]], %swift.opaque* %1, %C17generic_witnesses8BarClass* [[THIS]], %swift.type* %T)

struct BarStruct : Barrable {
  var x:Int
  func bar<V>(x:BarStruct, y:V) { }
}
// CHECK: define internal void @_Tnk_V17generic_witnesses9BarStruct3barfRS0_U__FT1xS0_1yQ__T_(%swift.opaque*, %swift.opaque*, %swift.opaque*, %swift.type* %T, %swift.type* %This) {
// CHECK:   [[X_ADDR:%.*]] = bitcast %swift.opaque* %0 to %V17generic_witnesses9BarStruct*
// CHECK:   [[X_X_ADDR:%.*]] = getelementptr inbounds %V17generic_witnesses9BarStruct* [[X_ADDR]], i32 0, i32 0
// CHECK:   [[X_X_VALUE_ADDR:%.*]] = getelementptr inbounds %Si* [[X_X_ADDR]], i32 0, i32 0
// CHECK:   [[X_X_VALUE:%.*]] = load i64* [[X_X_VALUE_ADDR]], align 8
// CHECK:   [[THIS_BYREF:%.*]] = bitcast %swift.opaque* %2 to %V17generic_witnesses9BarStruct*
// CHECK:   call void @_TV17generic_witnesses9BarStruct3barfRS0_U__FT1xS0_1yQ__T_(i64 [[X_X_VALUE]], %swift.opaque* %1, %V17generic_witnesses9BarStruct* [[THIS_BYREF]], %swift.type* %T)

// Force witness tables to be emitted.
var cbar : Barrable = BarClass()
var sbar : Barrable = BarStruct()

protocol HasAssociatedType {
  typealias Foo : Fooable
}

protocol Bassable {
  func bas<T:HasAssociatedType>(x:T, y:T.Foo)
}

class BasClass : Bassable {
  func bas<U:HasAssociatedType>(x:U, y:U.Foo) {}
}
// CHECK: define internal void @_Tnk_C17generic_witnesses8BasClass3basfS0_US_17HasAssociatedType_S_7Fooable__FT1xQ_1yQ0__T_(%swift.opaque*, %swift.opaque*, %swift.opaque*, %swift.type* %T, i8** %T.HasAssociatedType, %swift.type* %T.Foo, i8** %T.Foo.Fooable, %swift.type* %This) {
// CHECK:   [[THIS_ADDR:%.*]] = bitcast %swift.opaque* %2 to %C17generic_witnesses8BasClass**
// CHECK:   [[THIS:%.*]] = load %C17generic_witnesses8BasClass** [[THIS_ADDR]], align 8
// CHECK:   [[THIS_REFCOUNTED:%.*]] = bitcast %C17generic_witnesses8BasClass* [[THIS]] to %swift.refcounted*
// CHECK:   call void @swift_retain_noresult(%swift.refcounted* [[THIS_REFCOUNTED]])
// CHECK:   call void @_TC17generic_witnesses8BasClass3basfS0_US_17HasAssociatedType_S_7Fooable__FT1xQ_1yQ0__T_(%swift.opaque* %0, %swift.opaque* %1, %C17generic_witnesses8BasClass* [[THIS]], %swift.type* %T, i8** %T.HasAssociatedType, %swift.type* %T.Foo, i8** %T.Foo.Fooable)

struct BasStruct : Bassable {
  func bas<V:HasAssociatedType>(x:V, y:V.Foo) {}
}
// No abstraction difference to protocol witness
// CHECK-NOT: define internal void @_Tnk_S17generic_witnesses9BasStruct3basfS0_US_17HasAssociatedType_S_7Fooable__FT1xQ_1yQ0__T_

// Force witness tables to be emitted.
var cbas : Bassable = BasClass()
var sbas : Bassable = BasStruct()
