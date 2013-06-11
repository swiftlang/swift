// RUN: %swift -emit-llvm -triple x86_64-apple-darwin10 %s | FileCheck %s

protocol [class_protocol] ClassBound { }
protocol NotClassBound { }

// CHECK: define %objc_object* @_T22class_bounded_generics23class_bounded_archetypeUS_10ClassBound__FT1xQ__Q_(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype<T:ClassBound>(x:T) -> T {
  // CHECK: call %objc_object* @objc_retain(%objc_object* {{%.*}})
  return x
}

// CHECK: define { %objc_object*, %objc_object* } @_T22class_bounded_generics29class_bounded_archetype_tupleUS_10ClassBound__FT1xQ__TQ_Q__(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype_tuple<T:ClassBound>(x:T) -> (T, T) {
  return (x, x)
}

class ConcreteClass : ClassBound {}

// CHECK: define %C22class_bounded_generics13ConcreteClass* @_T22class_bounded_generics28call_class_bounded_archetypeFT1xCS_13ConcreteClass_S0_(%C22class_bounded_generics13ConcreteClass*) {
func call_class_bounded_archetype(x:ConcreteClass) -> ConcreteClass {
  return class_bounded_archetype(x)
  // CHECK: [[IN:%.*]] = bitcast %C22class_bounded_generics13ConcreteClass* {{%.*}} to %objc_object*
  // CHECK: [[OUT_ORIG:%.*]] = call %objc_object* @_T22class_bounded_generics23class_bounded_archetypeUS_10ClassBound__FT1xQ__Q_(%objc_object* [[IN]], {{.*}})
  // CHECK: [[OUT:%.*]] = bitcast %objc_object* [[OUT_ORIG]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK: define void @_T22class_bounded_generics27not_class_bounded_archetypeUS_13NotClassBound__FT1xQ__Q_(%swift.opaque* noalias sret, %swift.opaque*, %swift.type* %T, i8** %T.NotClassBound)
func not_class_bounded_archetype<T:NotClassBound>(x:T) -> T {
  return x
}

// CHECK: define %objc_object* @_T22class_bounded_generics44class_bounded_archetype_to_not_class_boundedUS_10ClassBoundS_13NotClassBound__FT1xQ__Q_(%objc_object*, %swift.type* %T, i8** %T.ClassBound, i8** %T.NotClassBound) {
func class_bounded_archetype_to_not_class_bounded
<T:protocol<ClassBound, NotClassBound>>(x:T) -> T {
  // CHECK: alloca %objc_object*, align 8
  return not_class_bounded_archetype(x)
  // CHECK: [[IN_BUF:%.*]] = alloca %objc_object*, align 8
  // CHECK: [[OUT_BUF:%.*]] = alloca %objc_object*, align 8
  // CHECK: store %objc_object* {{%.*}}, %objc_object** [[IN_BUF]]
  // CHECK: [[IN_CAST:%.*]] = bitcast %objc_object** [[IN_BUF]] to %swift.opaque*
  // CHECK: [[OUT_CAST:%.*]] = bitcast %objc_object** [[OUT_BUF]] to %swift.opaque*
}
