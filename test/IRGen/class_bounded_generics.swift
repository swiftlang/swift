// RUN: %target-swift-frontend -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64
// XFAIL: linux

protocol ClassBound : class {
  func classBoundMethod()
}
protocol ClassBound2 : class {
  func classBoundMethod2()
}
protocol ClassBoundBinary : class, ClassBound {
  func classBoundBinaryMethod(x: Self)
}
@objc protocol ObjCClassBound {
  func objCClassBoundMethod()
}
@objc protocol ObjCClassBound2 {
  func objCClassBoundMethod2()
}
protocol NotClassBound {
  func notClassBoundMethod()
  func notClassBoundBinaryMethod(x: Self)
}

struct ClassGenericFieldStruct<T:ClassBound> {
  var x : Int
  var y : T
  var z : Int
}

struct ClassProtocolFieldStruct {
  var x : Int
  var y : ClassBound
  var z : Int
}

class ClassGenericFieldClass<T:ClassBound> {
  final var x : Int = 0
  final var y : T
  final var z : Int = 0

  init(t: T) {
    y = t
  }
}

class ClassProtocolFieldClass {
  var x : Int = 0
  var y : ClassBound
  var z : Int = 0

  init(classBound cb: ClassBound) {
    y = cb
  }
}

// CHECK: %C22class_bounded_generics22ClassGenericFieldClass = type <{ %swift.refcounted, %Si, %objc_object*, %Si }>
// CHECK: %V22class_bounded_generics24ClassProtocolFieldStruct = type <{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>
// CHECK: %V22class_bounded_generics23ClassGenericFieldStruct = type <{ %Si, %objc_object*, %Si }>

// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics23class_bounded_archetype{{.*}}(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype<T : ClassBound>(x: T) -> T {
  return x
}

class SomeClass {}
class SomeSubclass : SomeClass {}

// CHECK-LABEL: define hidden %C22class_bounded_generics9SomeClass* @_TF22class_bounded_generics28superclass_bounded_archetype{{.*}}(%C22class_bounded_generics9SomeClass*, %swift.type* %T)
func superclass_bounded_archetype<T : SomeClass>(x: T) -> T {
  return x
}

// CHECK-LABEL: define hidden { %C22class_bounded_generics9SomeClass*, %C22class_bounded_generics12SomeSubclass* } @_TF22class_bounded_generics33superclass_bounded_archetype_call{{.*}}(%C22class_bounded_generics9SomeClass*, %C22class_bounded_generics12SomeSubclass*)
func superclass_bounded_archetype_call(x: SomeClass, y: SomeSubclass) -> (SomeClass, SomeSubclass) {
  return (superclass_bounded_archetype(x),
          superclass_bounded_archetype(y));
  // CHECK: [[SOMECLASS_RESULT:%.*]] = call %C22class_bounded_generics9SomeClass* @_TF22class_bounded_generics28superclass_bounded_archetype{{.*}}(%C22class_bounded_generics9SomeClass* {{%.*}}, {{.*}})
  // CHECK: [[SOMESUPERCLASS_IN:%.*]] = bitcast %C22class_bounded_generics12SomeSubclass* {{%.*}} to %C22class_bounded_generics9SomeClass*
  // CHECK: [[SOMESUPERCLASS_RESULT:%.*]] = call %C22class_bounded_generics9SomeClass* @_TF22class_bounded_generics28superclass_bounded_archetype{{.*}}(%C22class_bounded_generics9SomeClass* [[SOMESUPERCLASS_IN]], {{.*}})
  // CHECK: bitcast %C22class_bounded_generics9SomeClass* [[SOMESUPERCLASS_RESULT]] to %C22class_bounded_generics12SomeSubclass*
}

// CHECK-LABEL: define hidden void @_TF22class_bounded_generics30class_bounded_archetype_method{{.*}}(%objc_object*, %objc_object*, %swift.type* %T, i8** %T.ClassBoundBinary)
func class_bounded_archetype_method<T : ClassBoundBinary>(x: T, y: T) {
  x.classBoundMethod()
  // CHECK: [[INHERITED:%.*]] = load i8** %T.ClassBoundBinary, align 8
  // CHECK: [[INHERITED_WTBL:%.*]] = bitcast i8* [[INHERITED]] to i8**
  // CHECK: [[WITNESS:%.*]] = load i8** [[INHERITED_WTBL]], align 8
  // CHECK: [[WITNESS_FUNC:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %swift.type*)
  // CHECK: call void [[WITNESS_FUNC]](%objc_object* %0, %swift.type* {{.*}})
  x.classBoundBinaryMethod(y)
  // CHECK: [[WITNESS_ENTRY:%.*]] = getelementptr inbounds i8** %T.ClassBoundBinary, i32 1
  // CHECK: [[WITNESS:%.*]] = load i8** [[WITNESS_ENTRY]], align 8
  // CHECK: call void bitcast (void (%swift.refcounted*)* @swift_unknownRetain to void (%objc_object*)*)(%objc_object* %0)
  // CHECK: call void bitcast (void (%swift.refcounted*)* @swift_unknownRetain to void (%objc_object*)*)(%objc_object* [[Y:%.*]])
  // CHECK: [[WITNESS_FUNC:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %objc_object*, %swift.type*)
  // CHECK: call void [[WITNESS_FUNC]](%objc_object* [[Y]], %objc_object* %0, %swift.type* {{.*}})
}

// CHECK-LABEL: define hidden { %objc_object*, %objc_object* } @_TF22class_bounded_generics29class_bounded_archetype_tuple{{.*}}(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype_tuple<T : ClassBound>(x: T) -> (T, T) {
  return (x, x)
}

class ConcreteClass : ClassBoundBinary, NotClassBound {
  func classBoundMethod() {}
  func classBoundBinaryMethod(x: ConcreteClass) {}
  func notClassBoundMethod() {}
  func notClassBoundBinaryMethod(x: ConcreteClass) {}
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_TF22class_bounded_generics28call_class_bounded_archetype{{.*}}(%C22class_bounded_generics13ConcreteClass*) {
func call_class_bounded_archetype(x: ConcreteClass) -> ConcreteClass {
  return class_bounded_archetype(x)
  // CHECK: [[IN:%.*]] = bitcast %C22class_bounded_generics13ConcreteClass* {{%.*}} to %objc_object*
  // CHECK: [[OUT_ORIG:%.*]] = call %objc_object* @_TF22class_bounded_generics23class_bounded_archetype{{.*}}(%objc_object* [[IN]], {{.*}})
  // CHECK: [[OUT:%.*]] = bitcast %objc_object* [[OUT_ORIG]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK: define hidden void @_TF22class_bounded_generics27not_class_bounded_archetype{{.*}}(%swift.opaque* noalias sret, %swift.opaque* noalias, %swift.type* %T, i8** %T.NotClassBound)
func not_class_bounded_archetype<T : NotClassBound>(x: T) -> T {
  return x
}

// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics44class_bounded_archetype_to_not_class_bounded{{.*}}(%objc_object*, %swift.type* %T, i8** %T.ClassBound, i8** %T.NotClassBound) {
func class_bounded_archetype_to_not_class_bounded
<T:protocol<ClassBound, NotClassBound>>(x:T) -> T {
  // CHECK: alloca %objc_object*, align 8
  return not_class_bounded_archetype(x)
}

/* TODO Abstraction remapping to non-class-bounded witnesses
func class_and_not_class_bounded_archetype_methods
<T:protocol<ClassBound, NotClassBound>>(x:T, y:T) {
  x.classBoundMethod()
  x.classBoundBinaryMethod(y)
  x.notClassBoundMethod()
  x.notClassBoundBinaryMethod(y)
}
*/

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_TF22class_bounded_generics21class_bounded_erasure{{.*}}(%C22class_bounded_generics13ConcreteClass*) {
func class_bounded_erasure(x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[INSTANCE_OPAQUE:%.*]] = bitcast %C22class_bounded_generics13ConcreteClass* [[INSTANCE:%.*]] to %objc_object*
  // CHECK: [[T0:%.*]] = insertvalue { %objc_object*, i8** } undef, %objc_object* [[INSTANCE_OPAQUE]], 0
  // CHECK: [[T1:%.*]] = insertvalue { %objc_object*, i8** } [[T0]], i8** getelementptr inbounds ([1 x i8*]* @_TWPC22class_bounded_generics13ConcreteClassS_10ClassBoundS_, i32 0, i32 0), 1
  // CHECK: ret { %objc_object*, i8** } [[T1]]
}

// CHECK-LABEL: define hidden void @_TF22class_bounded_generics29class_bounded_protocol_method{{.*}}(%objc_object*, i8**) {
func class_bounded_protocol_method(x: ClassBound) {
  x.classBoundMethod()
  // CHECK: [[WITNESS:%.*]] = load i8** [[WITNESS_TABLE:%.*]], align 8
  // CHECK: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %swift.type*)
  // CHECK: call void [[WITNESS_FN]](%objc_object* %0, %swift.type* {{.*}})
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_TF22class_bounded_generics28class_bounded_archetype_cast{{.*}}(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype_cast<T : ClassBound>(x: T) -> ConcreteClass {
  return x as! ConcreteClass
  // CHECK: [[IN_PTR:%.*]] = bitcast %objc_object* {{%.*}} to i8*
  // CHECK: [[T0:%.*]] = call %swift.type* @_TMaC22class_bounded_generics13ConcreteClass()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
  // CHECK: [[OUT_PTR:%.*]] = call i8* @swift_dynamicCastClassUnconditional(i8* [[IN_PTR]], i8* [[T1]])
  // CHECK: [[OUT:%.*]] = bitcast i8* [[OUT_PTR]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics38class_bounded_archetype_archetype_cast{{.*}}(%objc_object*, %swift.type* %T, i8** %T.ClassBound, %swift.type* %U, i8** %U.ClassBound)
func class_bounded_archetype_archetype_cast
<T:ClassBound, U:ClassBound>(x:T) -> U {
  return x as! U
  // CHECK: [[IN_PTR:%.*]] = bitcast %objc_object* {{%.*}} to i8*
  // CHECK: [[OUT_TYPE:%.*]] = bitcast %swift.type* %U to i8*
  // CHECK: [[OUT_PTR:%.*]] = call i8* @swift_dynamicCastUnknownClassUnconditional(i8* [[IN_PTR]], i8* [[OUT_TYPE]])
  // CHECK: [[OUT:%.*]] = bitcast i8* [[OUT_PTR]] to %objc_object*
  // CHECK: ret %objc_object* [[OUT]]
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_TF22class_bounded_generics27class_bounded_protocol_cast{{.*}}(%objc_object*, i8**)
func class_bounded_protocol_cast(x: ClassBound) -> ConcreteClass {
  return x as! ConcreteClass
  // CHECK: [[IN_PTR:%.*]] = bitcast %objc_object* {{%.*}} to i8*
  // CHECK: [[T0:%.*]] = call %swift.type* @_TMaC22class_bounded_generics13ConcreteClass()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
  // CHECK: [[OUT_PTR:%.*]] = call i8* @swift_dynamicCastClassUnconditional(i8* [[IN_PTR]], i8* [[T1]])
  // CHECK: [[OUT:%.*]] = bitcast i8* [[OUT_PTR]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_TF22class_bounded_generics35class_bounded_protocol_conversion{{.*}}(%objc_object*, i8**, i8**) {
func class_bounded_protocol_conversion_1(x: protocol<ClassBound, ClassBound2>)
-> ClassBound {
  return x
}
// CHECK-LABEL: define hidden { %objc_object*, i8** } @_TF22class_bounded_generics35class_bounded_protocol_conversion{{.*}}(%objc_object*, i8**, i8**) {
func class_bounded_protocol_conversion_2(x: protocol<ClassBound, ClassBound2>)
-> ClassBound2 {
  return x
}

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_TF22class_bounded_generics40objc_class_bounded_protocol_conversion{{.*}}(%objc_object*, i8**) {
func objc_class_bounded_protocol_conversion_1
(x:protocol<ClassBound, ObjCClassBound>) -> ClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics40objc_class_bounded_protocol_conversion{{.*}}(%objc_object*, i8**) {
func objc_class_bounded_protocol_conversion_2
(x:protocol<ClassBound, ObjCClassBound>) -> ObjCClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics40objc_class_bounded_protocol_conversion{{.*}}(%objc_object*)
func objc_class_bounded_protocol_conversion_3
(x:protocol<ObjCClassBound, ObjCClassBound2>) -> ObjCClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_TF22class_bounded_generics40objc_class_bounded_protocol_conversion{{.*}}(%objc_object*)
func objc_class_bounded_protocol_conversion_4
(x:protocol<ObjCClassBound, ObjCClassBound2>) -> ObjCClassBound2 {
  return x
}

// CHECK-LABEL: define hidden { i64, %objc_object*, i64 } @_TF22class_bounded_generics33class_generic_field_struct_fields{{.*}}(i64, %objc_object*, i64, %swift.type* %T, i8** %T.ClassBound)
func class_generic_field_struct_fields<T : ClassBound>
(x:ClassGenericFieldStruct<T>) -> (Int, T, Int) {
  return (x.x, x.y, x.z)
}

// CHECK-LABEL: define hidden void @_TF22class_bounded_generics34class_protocol_field_struct_fields{{.*}}(<{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>* noalias sret, i64, %objc_object*, i8**, i64)
func class_protocol_field_struct_fields
(x:ClassProtocolFieldStruct) -> (Int, ClassBound, Int) {
  return (x.x, x.y, x.z)
}

// CHECK-LABEL: define hidden { i64, %objc_object*, i64 } @_TF22class_bounded_generics32class_generic_field_class_fields{{.*}}(%C22class_bounded_generics22ClassGenericFieldClass*)
func class_generic_field_class_fields<T : ClassBound>
(x:ClassGenericFieldClass<T>) -> (Int, T, Int) {
  return (x.x, x.y, x.z)
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 1
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 2
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 3
}

// CHECK-LABEL: define hidden void @_TF22class_bounded_generics33class_protocol_field_class_fields{{.*}}(<{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>* noalias sret, %C22class_bounded_generics23ClassProtocolFieldClass*)
func class_protocol_field_class_fields(x: ClassProtocolFieldClass)
-> (Int, ClassBound, Int) {
  return (x.x, x.y, x.z)
  // CHECK:  = call i64 %{{[0-9]+}}
  // CHECK:  = call { %objc_object*, i8** } %{{[0-9]+}}
  // CHECK:  = call i64 %{{[0-9]+}}
}

class SomeSwiftClass {
  class func foo() {}
}

// T must have a Swift loayout, so we can load this metatype with a direct access.
// CHECK-LABEL: define hidden void @_TF22class_bounded_generics22class_bounded_metatype
// CHECK:      [[T0:%.*]] = getelementptr inbounds %C22class_bounded_generics14SomeSwiftClass* {{%.*}}, i32 0, i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load %swift.type** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast %swift.type* [[T1]] to void (%swift.type*)**
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds void (%swift.type*)** [[T2]], i64 9
// CHECK-NEXT: load void (%swift.type*)** [[T3]], align 8
func class_bounded_metatype<T: SomeSwiftClass>(t : T) {
  t.dynamicType.foo()
}

class WeakRef<T: AnyObject>  {
  weak var value: T?
}
