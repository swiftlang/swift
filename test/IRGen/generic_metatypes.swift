// RUN: %swift -target x86_64-apple-macosx10.9 -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s
// RUN: %swift -target i386-apple-ios7.0 -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 %s
// RUN: %swift -target x86_64-apple-ios7.0 -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s
// RUN: %swift -target armv7-apple-ios7.0 -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 %s
// RUN: %swift -target arm64-apple-ios7.0 -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s
// RUN: %swift -target x86_64-unknown-linux-gnu -disable-objc-interop -emit-ir -parse-stdlib -primary-file %s | FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s

// REQUIRES: X86

// CHECK: define hidden %swift.type* [[GENERIC_TYPEOF:@_TF17generic_metatypes13genericTypeof.*]](%swift.opaque* noalias, %swift.type* [[TYPE:%.*]])
func genericTypeof<T>(x: T) -> T.Type {
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* {{.*}}, %swift.type* [[TYPE]])
  // CHECK: ret %swift.type* [[METATYPE]]
  return x.dynamicType
}

struct Foo {}
class Bar {}

// CHECK: define hidden %swift.type* @_TF17generic_metatypes27remapToSubstitutedMetatypes{{.*}}(%C17generic_metatypes3Bar*) {
func remapToSubstitutedMetatypes(x: Foo, y: Bar)
  -> (Foo.Type, Bar.Type)
{
  // CHECK: call %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* undef, %swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}})
  // CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
  // CHECK: [[BAR_META:%.*]] = call %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* {{%.*}}, %swift.type* [[T0]])
  // CHECK: ret %swift.type* [[BAR_META]]
  return (genericTypeof(x), genericTypeof(y))
}


// CHECK: define hidden void @_TF17generic_metatypes23remapToGenericMetatypesFT_T_()
func remapToGenericMetatypes() {
  // CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
  // CHECK: [[T1:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
  // CHECK: call void @_TF17generic_metatypes16genericMetatypes{{.*}}(%swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type* [[T0]], %swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type* [[T1]])
  genericMetatypes(Foo.self, Bar.self)
}

func genericMetatypes<T, U>(t: T.Type, u: U.Type) {}

protocol Bas {}

// CHECK: define hidden { %swift.type*, i8** } @_TF17generic_metatypes14protocolTypeof{{.*}}(%P17generic_metatypes3Bas_* noalias)
func protocolTypeof(x: Bas) -> Bas.Type {
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %P17generic_metatypes3Bas_* [[X:%.*]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type** [[METADATA_ADDR]]
  // CHECK: [[BUFFER:%.*]] = getelementptr inbounds %P17generic_metatypes3Bas_* [[X]], i32 0, i32 0
  // CHECK: [[METADATA_I8:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
  // CHECK-32: [[VW_ADDR:%.*]] = getelementptr inbounds i8*** [[METADATA_I8]], i32 -1
  // CHECK-64: [[VW_ADDR:%.*]] = getelementptr inbounds i8*** [[METADATA_I8]], i64 -1
  // CHECK: [[VW:%.*]] = load i8*** [[VW_ADDR]]
  // CHECK: [[PROJECT_ADDR:%.*]] = getelementptr inbounds i8** [[VW]], i32 2
  // CHECK-32: [[PROJECT_PTR:%.*]] = load i8** [[PROJECT_ADDR]], align 4
  // CHECK-64: [[PROJECT_PTR:%.*]] = load i8** [[PROJECT_ADDR]], align 8
  // CHECK-32: [[PROJECT:%.*]] = bitcast i8* [[PROJECT_PTR]] to %swift.opaque* ([12 x i8]*, %swift.type*)*
  // CHECK-64: [[PROJECT:%.*]] = bitcast i8* [[PROJECT_PTR]] to %swift.opaque* ([24 x i8]*, %swift.type*)*
  // CHECK-32: [[PROJECTION:%.*]] = call %swift.opaque* [[PROJECT]]([12 x i8]* [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK-64: [[PROJECTION:%.*]] = call %swift.opaque* [[PROJECT]]([24 x i8]* [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* [[PROJECTION]], %swift.type* [[METADATA]])
  // CHECK: [[T0:%.*]] = getelementptr inbounds %P17generic_metatypes3Bas_* [[X]], i32 0, i32 2
  // CHECK-32: [[WTABLE:%.*]] = load i8*** [[T0]], align 4
  // CHECK-64: [[WTABLE:%.*]] = load i8*** [[T0]], align 8
  // CHECK: [[T0:%.*]] = insertvalue { %swift.type*, i8** } undef, %swift.type* [[METATYPE]], 0
  // CHECK: [[T1:%.*]] = insertvalue { %swift.type*, i8** } [[T0]], i8** [[WTABLE]], 1
  // CHECK: ret { %swift.type*, i8** } [[T1]]
  return x.dynamicType
}

// FIXME: The typechecker doesn't support this conversion.
/*
struct Zim : Bas {}
class Zang : Bas {}

func metatypeErasure(z: Zim.Type) -> Bas.Type {
  return z
}
func metatypeErasure(z: Zang.Type) -> Bas.Type {
  return z
}
*/

struct OneArg<T> {}
struct TwoArgs<T, U> {}
struct ThreeArgs<T, U, V> {}
struct FourArgs<T, U, V, W> {}
struct FiveArgs<T, U, V, W, X> {}

func genericMetatype<A>(x: A.Type) {}

// CHECK-LABEL: define hidden void @_TF17generic_metatypes20makeGenericMetatypesFT_T_() {
func makeGenericMetatypes() {
  // CHECK: call %swift.type* @_TMaGV17generic_metatypes6OneArgVS_3Foo_() [[NOUNWIND_READNONE:#[0-9]+]]
  genericMetatype(OneArg<Foo>.self)
  
  // CHECK: call %swift.type* @_TMaGV17generic_metatypes7TwoArgsVS_3FooCS_3Bar_() [[NOUNWIND_READNONE]]
  genericMetatype(TwoArgs<Foo, Bar>.self)

  // CHECK: call %swift.type* @_TMaGV17generic_metatypes9ThreeArgsVS_3FooCS_3BarS1__() [[NOUNWIND_READNONE]]
  genericMetatype(ThreeArgs<Foo, Bar, Foo>.self)

  // CHECK: call %swift.type* @_TMaGV17generic_metatypes8FourArgsVS_3FooCS_3BarS1_S2__() [[NOUNWIND_READNONE]]
  genericMetatype(FourArgs<Foo, Bar, Foo, Bar>.self)

  // CHECK: call %swift.type* @_TMaGV17generic_metatypes8FiveArgsVS_3FooCS_3BarS1_S2_S1__() [[NOUNWIND_READNONE]]
  genericMetatype(FiveArgs<Foo, Bar, Foo, Bar, Foo>.self)
}

// CHECK: define linkonce_odr hidden %swift.type* @_TMaGV17generic_metatypes6OneArgVS_3Foo_() [[NOUNWIND_READNONE]]
// CHECK:   call %swift.type* @swift_getGenericMetadata1(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes6OneArg {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

// CHECK: define linkonce_odr hidden %swift.type* @_TMaGV17generic_metatypes7TwoArgsVS_3FooCS_3Bar_() [[NOUNWIND_READNONE]]
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
// CHECK: call %swift.type* @swift_getGenericMetadata2(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes7TwoArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* [[T1]]) [[NOUNWIND_READNONE]]

// CHECK: define linkonce_odr hidden %swift.type* @_TMaGV17generic_metatypes9ThreeArgsVS_3FooCS_3BarS1__() [[NOUNWIND_READNONE]]
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
// CHECK: call %swift.type* @swift_getGenericMetadata3(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes9ThreeArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* [[T1]], i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}) [[NOUNWIND_READNONE]]

// CHECK: define linkonce_odr hidden %swift.type* @_TMaGV17generic_metatypes8FourArgsVS_3FooCS_3BarS1_S2__() [[NOUNWIND_READNONE]]
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK: [[T1:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK: [[T2:%.*]] = bitcast %swift.type* [[T0]] to i8*
// CHECK: [[T3:%.*]] = bitcast %swift.type* [[T1]] to i8*
// CHECK: call %swift.type* @swift_getGenericMetadata4(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes8FourArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* [[T2]], i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* [[T3]]) [[NOUNWIND_READNONE]]

// CHECK: define linkonce_odr hidden %swift.type* @_TMaGV17generic_metatypes8FiveArgsVS_3FooCS_3BarS1_S2_S1__() [[NOUNWIND_READNONE]]
// CHECK:   [[BUFFER:%.*]] = alloca [[BUFFER_T:.*]], align
// CHECK:   [[BAR0:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK:   [[BAR1:%.*]] = call %swift.type* @_TMaC17generic_metatypes3Bar()
// CHECK:   [[T0:%.*]] = getelementptr inbounds [[BUFFER_T]]* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* getelementptr {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type** [[T0]]
// CHECK:   [[T0:%.*]] = getelementptr inbounds [[BUFFER_T]]* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* [[BAR0]], %swift.type** [[T0]]
// CHECK:   [[T0:%.*]] = getelementptr inbounds [[BUFFER_T]]* [[BUFFER]], i32 0, i32 2
// CHECK:   store %swift.type* getelementptr {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type** [[T0]]
// CHECK:   [[T0:%.*]] = getelementptr inbounds [[BUFFER_T]]* [[BUFFER]], i32 0, i32 3
// CHECK:   store %swift.type* [[BAR1]], %swift.type** [[T0]]
// CHECK:   [[T0:%.*]] = getelementptr inbounds [[BUFFER_T]]* [[BUFFER]], i32 0, i32 4
// CHECK:   store %swift.type* getelementptr {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type** [[T0]]

// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }
