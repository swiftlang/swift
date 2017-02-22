// RUN: %swift -Xllvm -new-mangling-for-tests -target armv7-apple-ios7.0 -module-name generic_metatypes -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 %s
// RUN: %swift -Xllvm -new-mangling-for-tests -target arm64-apple-ios7.0 -emit-ir -module-name generic_metatypes -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s
// RUN: %swift -Xllvm -new-mangling-for-tests -target armv7-apple-tvos9.0 -emit-ir -module-name generic_metatypes -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 %s
// RUN: %swift -Xllvm -new-mangling-for-tests -target arm64-apple-tvos9.0 -emit-ir -module-name generic_metatypes -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 %s
// RUN: %swift -Xllvm -new-mangling-for-tests -target armv7k-apple-watchos2.0 -emit-ir -module-name generic_metatypes -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 %s

// REQUIRES: CODEGENERATOR=ARM

// CHECK: define hidden swiftcc %swift.type* [[GENERIC_TYPEOF:@_T017generic_metatypes0A6TypeofxmxlF]](%swift.opaque* noalias nocapture, %swift.type* [[TYPE:%.*]])
func genericTypeof<T>(_ x: T) -> T.Type {
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* {{.*}}, %swift.type* [[TYPE]], i1 false)
  // CHECK: ret %swift.type* [[METATYPE]]
  return type(of: x)
}

struct Foo {}
class Bar {}

// CHECK-LABEL: define hidden swiftcc %swift.type* @_T017generic_metatypes27remapToSubstitutedMetatypes{{.*}}(%T17generic_metatypes3BarC*) {{.*}} {
func remapToSubstitutedMetatypes(_ x: Foo, y: Bar)
  -> (Foo.Type, Bar.Type)
{
  // CHECK: call swiftcc %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* noalias nocapture undef, %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}})
  // CHECK: [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
  // CHECK: [[BAR_META:%.*]] = call swiftcc %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* noalias nocapture {{%.*}}, %swift.type* [[T0]])
  // CHECK: ret %swift.type* [[BAR_META]]
  return (genericTypeof(x), genericTypeof(y))
}


// CHECK-LABEL: define hidden swiftcc void @_T017generic_metatypes23remapToGenericMetatypesyyF()
func remapToGenericMetatypes() {
  // CHECK: [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
  // CHECK: call swiftcc void @_T017generic_metatypes0A9Metatypes{{.*}}(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}} %swift.type* [[T0]], %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}} %swift.type* [[T0]])
  genericMetatypes(Foo.self, Bar.self)
}

func genericMetatypes<T, U>(_ t: T.Type, _ u: U.Type) {}

protocol Bas {}

// CHECK: define hidden swiftcc { %swift.type*, i8** } @_T017generic_metatypes14protocolTypeof{{.*}}(%T17generic_metatypes3BasP* noalias nocapture dereferenceable({{.*}}))
func protocolTypeof(_ x: Bas) -> Bas.Type {
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* [[X:%.*]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]
  // CHECK: [[BUFFER:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* [[X]], i32 0, i32 0
  // CHECK: [[METADATA_I8:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
  // CHECK-32: [[VW_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_I8]], i32 -1
  // CHECK-64: [[VW_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_I8]], i64 -1
  // CHECK: [[VW:%.*]] = load i8**, i8*** [[VW_ADDR]]
  // CHECK: [[PROJECT_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VW]], i32 2
  // CHECK-32: [[PROJECT_PTR:%.*]] = load i8*, i8** [[PROJECT_ADDR]], align 4
  // CHECK-64: [[PROJECT_PTR:%.*]] = load i8*, i8** [[PROJECT_ADDR]], align 8
  // CHECK-32: [[PROJECT:%.*]] = bitcast i8* [[PROJECT_PTR]] to %swift.opaque* ([12 x i8]*, %swift.type*)*
  // CHECK-64: [[PROJECT:%.*]] = bitcast i8* [[PROJECT_PTR]] to %swift.opaque* ([24 x i8]*, %swift.type*)*
  // CHECK-32: [[PROJECTION:%.*]] = call %swift.opaque* [[PROJECT]]([12 x i8]* [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK-64: [[PROJECTION:%.*]] = call %swift.opaque* [[PROJECT]]([24 x i8]* [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* [[PROJECTION]], %swift.type* [[METADATA]], i1 true)
  // CHECK: [[T0:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* [[X]], i32 0, i32 2
  // CHECK-32: [[WTABLE:%.*]] = load i8**, i8*** [[T0]], align 4
  // CHECK-64: [[WTABLE:%.*]] = load i8**, i8*** [[T0]], align 8
  // CHECK: [[T0:%.*]] = insertvalue { %swift.type*, i8** } undef, %swift.type* [[METATYPE]], 0
  // CHECK: [[T1:%.*]] = insertvalue { %swift.type*, i8** } [[T0]], i8** [[WTABLE]], 1
  // CHECK: ret { %swift.type*, i8** } [[T1]]
  return type(of: x)
}

struct Zim : Bas {}
class Zang : Bas {}

// CHECK-LABEL: define hidden swiftcc { %swift.type*, i8** } @_T017generic_metatypes15metatypeErasureAA3Bas_pXpAA3ZimVmF() #0
func metatypeErasure(_ z: Zim.Type) -> Bas.Type {
  // CHECK: ret { %swift.type*, i8** } {{.*}} @_T017generic_metatypes3ZimVMf, {{.*}} @_T017generic_metatypes3ZimVAA3BasAAWP
  return z
}

// CHECK-LABEL: define hidden swiftcc { %swift.type*, i8** } @_T017generic_metatypes15metatypeErasureAA3Bas_pXpAA4ZangCmF(%swift.type*) #0
func metatypeErasure(_ z: Zang.Type) -> Bas.Type {
  // CHECK: [[RET:%.*]] = insertvalue { %swift.type*, i8** } undef, %swift.type* %0, 0
  // CHECK: [[RET2:%.*]] = insertvalue { %swift.type*, i8** } [[RET]], i8** getelementptr inbounds ([0 x i8*], [0 x i8*]* @_T017generic_metatypes4ZangCAA3BasAAWP, i32 0, i32 0), 1
  // CHECK: ret { %swift.type*, i8** } [[RET2]]
  return z
}

struct OneArg<T> {}
struct TwoArgs<T, U> {}
struct ThreeArgs<T, U, V> {}
struct FourArgs<T, U, V, W> {}
struct FiveArgs<T, U, V, W, X> {}

func genericMetatype<A>(_ x: A.Type) {}

// CHECK-LABEL: define hidden swiftcc void @_T017generic_metatypes20makeGenericMetatypesyyF() {{.*}} {
func makeGenericMetatypes() {
  // CHECK: call %swift.type* @_T017generic_metatypes6OneArgVyAA3FooVGMa() [[NOUNWIND_READNONE:#[0-9]+]]
  genericMetatype(OneArg<Foo>.self)

  // CHECK: call %swift.type* @_T017generic_metatypes7TwoArgsVyAA3FooVAA3BarCGMa() [[NOUNWIND_READNONE]]
  genericMetatype(TwoArgs<Foo, Bar>.self)

  // CHECK: call %swift.type* @_T017generic_metatypes9ThreeArgsVyAA3FooVAA3BarCAEGMa() [[NOUNWIND_READNONE]]
  genericMetatype(ThreeArgs<Foo, Bar, Foo>.self)

  // CHECK: call %swift.type* @_T017generic_metatypes8FourArgsVyAA3FooVAA3BarCAeGGMa() [[NOUNWIND_READNONE]]
  genericMetatype(FourArgs<Foo, Bar, Foo, Bar>.self)

  // CHECK: call %swift.type* @_T017generic_metatypes8FiveArgsVyAA3FooVAA3BarCAegEGMa() [[NOUNWIND_READNONE]]
  genericMetatype(FiveArgs<Foo, Bar, Foo, Bar, Foo>.self)
}

// CHECK: define linkonce_odr hidden %swift.type* @_T017generic_metatypes6OneArgVyAA3FooVGMa() [[NOUNWIND_READNONE_OPT:#[0-9]+]]
// CHECK:   call %swift.type* @_T017generic_metatypes6OneArgVMa(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

// CHECK-LABEL: define hidden %swift.type* @_T017generic_metatypes6OneArgVMa(%swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type* }, { %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %0, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_rt_swift_getGenericMetadata(%swift.type_pattern* {{.*}} @_T017generic_metatypes6OneArgVMP {{.*}}, i8* [[BUFFER_PTR]])
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.end
// CHECK:   ret %swift.type* [[METADATA]]

// CHECK: define linkonce_odr hidden %swift.type* @_T017generic_metatypes7TwoArgsVyAA3FooVAA3BarCGMa() [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
// CHECK:   call %swift.type* @_T017generic_metatypes7TwoArgsVMa(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]])

// CHECK-LABEL: define hidden %swift.type* @_T017generic_metatypes7TwoArgsVMa(%swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type* }, { %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %0, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type* }, { %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_rt_swift_getGenericMetadata(%swift.type_pattern* {{.*}} @_T017generic_metatypes7TwoArgsVMP {{.*}}, i8* [[BUFFER_PTR]])
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.end
// CHECK:   ret %swift.type* [[METADATA]]

// CHECK: define linkonce_odr hidden %swift.type* @_T017generic_metatypes9ThreeArgsVyAA3FooVAA3BarCAEGMa() [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
// CHECK:   call %swift.type* @_T017generic_metatypes9ThreeArgsVMa(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]], %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}) [[NOUNWIND_READNONE]]

// CHECK-LABEL: define hidden %swift.type* @_T017generic_metatypes9ThreeArgsVMa(%swift.type*, %swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %0, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 2
// CHECK:   store %swift.type* %2, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_rt_swift_getGenericMetadata(%swift.type_pattern* {{.*}} @_T017generic_metatypes9ThreeArgsVMP {{.*}}, i8* [[BUFFER_PTR]])
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.end
// CHECK:   ret %swift.type* [[METADATA]]

// CHECK: define linkonce_odr hidden %swift.type* @_T017generic_metatypes8FourArgsVyAA3FooVAA3BarCAeGGMa() [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
// CHECK:   call %swift.type* @_T017generic_metatypes8FourArgsVMa(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]], %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]]) [[NOUNWIND_READNONE]]

// CHECK-LABEL: define hidden %swift.type* @_T017generic_metatypes8FourArgsVMa(%swift.type*, %swift.type*, %swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type*, %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %0, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 2
// CHECK:   store %swift.type* %2, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 3
// CHECK:   store %swift.type* %3, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_rt_swift_getGenericMetadata(%swift.type_pattern* {{.*}} @_T017generic_metatypes8FourArgsVMP {{.*}}, i8* [[BUFFER_PTR]])
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.end
// CHECK:   ret %swift.type* [[METADATA]]

// CHECK: define linkonce_odr hidden %swift.type* @_T017generic_metatypes8FiveArgsVyAA3FooVAA3BarCAegEGMa() [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call %swift.type* @_T017generic_metatypes3BarCMa()
// CHECK:   call %swift.type* @_T017generic_metatypes8FiveArgsVMa(%swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]], %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}, %swift.type* [[T0]], %swift.type* {{.*}} @_T017generic_metatypes3FooVMf, {{.*}}) [[NOUNWIND_READNONE]]

// CHECK-LABEL: define hidden %swift.type* @_T017generic_metatypes8FiveArgsVMa(%swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %0, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 2
// CHECK:   store %swift.type* %2, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 3
// CHECK:   store %swift.type* %3, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_rt_swift_getGenericMetadata(%swift.type_pattern* {{.*}} @_T017generic_metatypes8FiveArgsVMP {{.*}}, i8* [[BUFFER_PTR]])
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.end
// CHECK:   ret %swift.type* [[METADATA]]

// CHECK: attributes [[NOUNWIND_READNONE_OPT]] = { nounwind readnone "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }
