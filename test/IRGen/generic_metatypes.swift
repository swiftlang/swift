// RUN: %swift -emit-ir -parse-stdlib -target x86_64-apple-darwin10 %s | FileCheck %s

// CHECK: define %swift.type* [[GENERIC_TYPEOF:@_TF17generic_metatypes13genericTypeof.*]](%swift.opaque* noalias, %swift.type* [[TYPE:%.*]])
func genericTypeof<T>(x: T) -> T.Type {
  // CHECK: [[TYPEOF_WITNESS_ADDR:%.*]] = getelementptr inbounds i8** {{%.*}}, i32 12
  // CHECK: [[TYPEOF_WITNESS:%.*]] = load i8** [[TYPEOF_WITNESS_ADDR]], align 8
  // CHECK: %typeof = bitcast i8* [[TYPEOF_WITNESS]] to %swift.type* (%swift.opaque*, %swift.type*)*
  // CHECK: [[METATYPE:%.*]] = call %swift.type* %typeof(%swift.opaque* {{.*}}, %swift.type* [[TYPE]])
  // CHECK: ret %swift.type* [[METATYPE]]
  return x.dynamicType
}

struct Foo {}
class Bar {}

// CHECK: define %swift.type* @_TF17generic_metatypes27remapToSubstitutedMetatypes{{.*}}(%C17generic_metatypes3Bar*) {
func remapToSubstitutedMetatypes(x: Foo, y: Bar)
  -> (Foo.Type, Bar.Type)
{
  // CHECK: call %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* undef, %swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}})
  // CHECK: [[BAR_META:%.*]] = call %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* {{%.*}}, %swift.type* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}})
  // CHECK: ret %swift.type* [[BAR_META]]
  return (genericTypeof(x), genericTypeof(y))
}


// CHECK: define void @_TF17generic_metatypes23remapToGenericMetatypesFT_T_()
func remapToGenericMetatypes() {
  // CHECK: call void @_TF17generic_metatypes16genericMetatypes{{.*}}(%swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}}, %swift.type* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, %swift.type* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}})
  genericMetatypes(Foo.self, Bar.self)
}

func genericMetatypes<T, U>(t: T.Type, u: U.Type) {}

protocol Bas {}

// CHECK: define %swift.type* @_TF17generic_metatypes14protocolTypeof{{.*}}(%P17generic_metatypes3Bas_* noalias)
func protocolTypeof(x: Bas) -> Bas.Type {
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %P17generic_metatypes3Bas_* [[X:%.*]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type** [[METADATA_ADDR]]
  // CHECK: [[BUFFER:%.*]] = getelementptr inbounds %P17generic_metatypes3Bas_* [[X]], i32 0, i32 0
  // CHECK: [[METADATA_I8:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
  // CHECK: [[VW_ADDR:%.*]] = getelementptr inbounds i8*** [[METADATA_I8]], i64 -1
  // CHECK: [[VW:%.*]] = load i8*** [[VW_ADDR]]
  // CHECK: [[PROJECT_ADDR:%.*]] = getelementptr inbounds i8** [[VW]], i32 2
  // CHECK: [[PROJECT_PTR:%.*]] = load i8** [[PROJECT_ADDR]], align 8
  // CHECK: [[PROJECT:%.*]] = bitcast i8* [[PROJECT_PTR]] to %swift.opaque* ([24 x i8]*, %swift.type*)*
  // CHECK: [[PROJECTION:%.*]] = call %swift.opaque* [[PROJECT]]([24 x i8]* [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK: [[METADATA_I8:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
  // CHECK: [[VW_ADDR:%.*]] = getelementptr inbounds i8*** [[METADATA_I8]], i64 -1
  // CHECK: [[VW:%.*]] = load i8*** [[VW_ADDR]]
  // CHECK: [[TYPEOF_ADDR:%.*]] = getelementptr inbounds i8** [[VW]], i32 12
  // CHECK: [[TYPEOF_PTR:%.*]] = load i8** [[TYPEOF_ADDR]], align 8
  // CHECK: [[TYPEOF:%.*]] = bitcast i8* [[TYPEOF_PTR]] to %swift.type* (%swift.opaque*, %swift.type*)*
  // CHECK: [[METATYPE:%.*]] = call %swift.type* [[TYPEOF]](%swift.opaque* [[PROJECTION]], %swift.type* [[METADATA]])
  // CHECK: ret %swift.type* [[METATYPE]]
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

// CHECK-LABEL: define void @_TF17generic_metatypes20makeGenericMetatypesFT_T_() {
func makeGenericMetatypes() {
  // CHECK: call %swift.type* @swift_getGenericMetadata1(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes6OneArg {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]
  genericMetatype(OneArg<Foo>.self)
  // CHECK: call %swift.type* @swift_getGenericMetadata2(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes7TwoArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}}) [[NOUNWIND_READNONE]]
  genericMetatype(TwoArgs<Foo, Bar>.self)
  // CHECK: call %swift.type* @swift_getGenericMetadata3(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes9ThreeArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}) [[NOUNWIND_READNONE]]
  genericMetatype(ThreeArgs<Foo, Bar, Foo>.self)
  // CHECK: call %swift.type* @swift_getGenericMetadata4(%swift.type_pattern* {{.*}} @_TMPdV17generic_metatypes8FourArgs {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}}, i8* {{.*}} @_TMdV17generic_metatypes3Foo {{.*}}, i8* {{.*}} @_TMdC17generic_metatypes3Bar {{.*}}) [[NOUNWIND_READNONE]]
  genericMetatype(FourArgs<Foo, Bar, Foo, Bar>.self)
  genericMetatype(FiveArgs<Foo, Bar, Foo, Bar, Foo>.self)
}

// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }
