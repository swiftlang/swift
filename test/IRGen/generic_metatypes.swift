
// RUN: %swift -module-name generic_metatypes -target x86_64-apple-macosx10.9  -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64 %s
// RUN: %swift -module-name generic_metatypes -target i386-apple-ios7.0        -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32 %s
// RUN: %swift -module-name generic_metatypes -target x86_64-apple-ios7.0      -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64  %s
// RUN: %swift -module-name generic_metatypes -target i386-apple-tvos9.0       -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32  %s
// RUN: %swift -module-name generic_metatypes -target x86_64-apple-tvos9.0     -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64  %s
// RUN: %swift -module-name generic_metatypes -target i386-apple-watchos2.0    -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32  %s
// RUN: %swift -module-name generic_metatypes -target x86_64-unknown-linux-gnu -disable-objc-interop -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64 %s

// RUN: %swift -module-name generic_metatypes -target armv7-apple-ios7.0       -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32 %s
// RUN: %swift -module-name generic_metatypes -target arm64-apple-ios7.0       -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64 %s
// RUN: %swift -module-name generic_metatypes -target armv7-apple-tvos9.0      -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32 %s
// RUN: %swift -module-name generic_metatypes -target arm64-apple-tvos9.0      -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-64 -DINT=i64 %s
// RUN: %swift -module-name generic_metatypes -target armv7k-apple-watchos2.0  -emit-ir -parse-stdlib -primary-file %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-32 -DINT=i32 %s

// REQUIRES: CODEGENERATOR=X86
// REQUIRES: CODEGENERATOR=ARM

enum Never {}

func never() -> Never { return never() }

@_semantics("typechecker.type(of:)")
public func type<T, Metatype>(of value: T) -> Metatype {
  never()
}

// CHECK: define hidden swiftcc %swift.type* [[GENERIC_TYPEOF:@"\$S17generic_metatypes0A6TypeofyxmxlF"]](%swift.opaque* noalias nocapture, %swift.type* [[TYPE:%.*]])
func genericTypeof<T>(_ x: T) -> T.Type {
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* {{.*}}, %swift.type* [[TYPE]], i1 false)
  // CHECK: ret %swift.type* [[METATYPE]]
  return type(of: x)
}

struct Foo {}
class Bar {}

// CHECK-LABEL: define hidden swiftcc %swift.type* @"$S17generic_metatypes27remapToSubstitutedMetatypes{{.*}}"(%T17generic_metatypes3BarC*) {{.*}} {
func remapToSubstitutedMetatypes(_ x: Foo, y: Bar)
  -> (Foo.Type, Bar.Type)
{
  // CHECK: call swiftcc %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* noalias nocapture undef, %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}})
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 0)
  // CHECK: [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: [[BAR_META:%.*]] = call swiftcc %swift.type* [[GENERIC_TYPEOF]](%swift.opaque* noalias nocapture {{%.*}}, %swift.type* [[BAR]])
  // CHECK: ret %swift.type* [[BAR_META]]
  return (genericTypeof(x), genericTypeof(y))
}


// CHECK-LABEL: define hidden swiftcc void @"$S17generic_metatypes23remapToGenericMetatypesyyF"()
func remapToGenericMetatypes() {
  // CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 0)
  // CHECK: [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
  // CHECK: call swiftcc void @"$S17generic_metatypes0A9Metatypes{{.*}}"(%swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}} %swift.type* [[BAR]], %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}} %swift.type* [[BAR]])
  genericMetatypes(Foo.self, Bar.self)
}

func genericMetatypes<T, U>(_ t: T.Type, _ u: U.Type) {}

protocol Bas {}

// CHECK: define hidden swiftcc { %swift.type*, i8** } @"$S17generic_metatypes14protocolTypeof{{.*}}"(%T17generic_metatypes3BasP* noalias nocapture dereferenceable({{.*}}))
func protocolTypeof(_ x: Bas) -> Bas.Type {
  // CHECK: [[METADATA_ADDR:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* [[X:%.*]], i32 0, i32 1
  // CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** [[METADATA_ADDR]]
  // CHECK: [[BUFFER:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* [[X]], i32 0, i32 0
  // CHECK: [[VALUE_ADDR:%.*]] = call %swift.opaque* @__swift_project_boxed_opaque_existential_1({{.*}} [[BUFFER]], %swift.type* [[METADATA]])
  // CHECK: [[METATYPE:%.*]] = call %swift.type* @swift_getDynamicType(%swift.opaque* [[VALUE_ADDR]], %swift.type* [[METADATA]], i1 true)
  // CHECK: [[WTABLE_ADDR:%.*]] = getelementptr inbounds %T17generic_metatypes3BasP, %T17generic_metatypes3BasP* %0, i32 0, i32 2
  // CHECK: [[WTABLE:%.*]] = load i8**, i8*** [[WTABLE_ADDR]]
  // CHECK-NOT: call void @__swift_destroy_boxed_opaque_existential_1(%T17generic_metatypes3BasP* %0)
  // CHECK: [[T0:%.*]] = insertvalue { %swift.type*, i8** } undef, %swift.type* [[METATYPE]], 0
  // CHECK: [[T1:%.*]] = insertvalue { %swift.type*, i8** } [[T0]], i8** [[WTABLE]], 1
  // CHECK: ret { %swift.type*, i8** } [[T1]]
  return type(of: x)
}

struct Zim : Bas {}
class Zang : Bas {}

// CHECK-LABEL: define hidden swiftcc { %swift.type*, i8** } @"$S17generic_metatypes15metatypeErasureyAA3Bas_pXpAA3ZimVmF"() #0
func metatypeErasure(_ z: Zim.Type) -> Bas.Type {
  // CHECK: ret { %swift.type*, i8** } {{.*}} @"$S17generic_metatypes3ZimVMf", {{.*}} @"$S17generic_metatypes3ZimVAA3BasAAWP"
  return z
}

// CHECK-LABEL: define hidden swiftcc { %swift.type*, i8** } @"$S17generic_metatypes15metatypeErasureyAA3Bas_pXpAA4ZangCmF"(%swift.type*) #0
func metatypeErasure(_ z: Zang.Type) -> Bas.Type {
  // CHECK: [[RET:%.*]] = insertvalue { %swift.type*, i8** } undef, %swift.type* %0, 0
  // CHECK: [[RET2:%.*]] = insertvalue { %swift.type*, i8** } [[RET]], i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$S17generic_metatypes4ZangCAA3BasAAWP", i32 0, i32 0), 1
  // CHECK: ret { %swift.type*, i8** } [[RET2]]
  return z
}

struct OneArg<T> {}
struct TwoArgs<T, U> {}
struct ThreeArgs<T, U, V> {}
struct FourArgs<T, U, V, W> {}
struct FiveArgs<T, U, V, W, X> {}

func genericMetatype<A>(_ x: A.Type) {}

// CHECK-LABEL: define hidden swiftcc void @"$S17generic_metatypes20makeGenericMetatypesyyF"() {{.*}} {
func makeGenericMetatypes() {
  // CHECK: call swiftcc %swift.metadata_response @"$S17generic_metatypes6OneArgVyAA3FooVGMa"([[INT]] 0) [[NOUNWIND_READNONE:#[0-9]+]]
  genericMetatype(OneArg<Foo>.self)

  // CHECK: call swiftcc %swift.metadata_response @"$S17generic_metatypes7TwoArgsVyAA3FooVAA3BarCGMa"([[INT]] 0) [[NOUNWIND_READNONE]]
  genericMetatype(TwoArgs<Foo, Bar>.self)

  // CHECK: call swiftcc %swift.metadata_response @"$S17generic_metatypes9ThreeArgsVyAA3FooVAA3BarCAEGMa"([[INT]] 0) [[NOUNWIND_READNONE]]
  genericMetatype(ThreeArgs<Foo, Bar, Foo>.self)

  // CHECK: call swiftcc %swift.metadata_response @"$S17generic_metatypes8FourArgsVyAA3FooVAA3BarCAeGGMa"([[INT]] 0) [[NOUNWIND_READNONE]]
  genericMetatype(FourArgs<Foo, Bar, Foo, Bar>.self)

  // CHECK: call swiftcc %swift.metadata_response @"$S17generic_metatypes8FiveArgsVyAA3FooVAA3BarCAegEGMa"([[INT]] 0) [[NOUNWIND_READNONE]]
  genericMetatype(FiveArgs<Foo, Bar, Foo, Bar, Foo>.self)
}

// CHECK: define linkonce_odr hidden swiftcc %swift.metadata_response @"$S17generic_metatypes6OneArgVyAA3FooVGMa"([[INT]]) [[NOUNWIND_READNONE_OPT:#[0-9]+]]
// CHECK:   call swiftcc %swift.metadata_response @"$S17generic_metatypes6OneArgVMa"([[INT]] %0, %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}}) [[NOUNWIND_READNONE:#[0-9]+]]

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S17generic_metatypes6OneArgVMa"
// CHECK-SAME:    ([[INT]], %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type* }, { %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @swift_getGenericMetadata([[INT]] %0, i8* [[BUFFER_PTR]], %swift.type_descriptor* {{.*}} @"$S17generic_metatypes6OneArgVMn" {{.*}})
// CHECK:   [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$S17generic_metatypes7TwoArgsVyAA3FooVAA3BarCGMa"
// CHECK-SAME:    ([[INT]]) [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 255)
// CHECK:   [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:   call swiftcc %swift.metadata_response @"$S17generic_metatypes7TwoArgsVMa"([[INT]] %0, %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}}, %swift.type* [[BAR]])

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S17generic_metatypes7TwoArgsVMa"
// CHECK-SAME:    ([[INT]], %swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type* }, { %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type* }, { %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %2, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @swift_getGenericMetadata([[INT]] %0, i8* [[BUFFER_PTR]], %swift.type_descriptor* {{.*}} @"$S17generic_metatypes7TwoArgsVMn" {{.*}})
// CHECK:   [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$S17generic_metatypes9ThreeArgsVyAA3FooVAA3BarCAEGMa"
// CHECK-SAME:    ([[INT]]) [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 255)
// CHECK:   [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:   call swiftcc %swift.metadata_response @"$S17generic_metatypes9ThreeArgsVMa"([[INT]] %0, %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}}, %swift.type* [[BAR]], %swift.type* {{.*}} @"$S17generic_metatypes3FooVMf", {{.*}}) [[NOUNWIND_READNONE]]

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S17generic_metatypes9ThreeArgsVMa"
// CHECK-SAME:    ({{i[0-9]+}}, %swift.type*, %swift.type*, %swift.type*)
// CHECK:   [[BUFFER:%.*]] = alloca { %swift.type*, %swift.type*, %swift.type* }
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   call void @llvm.lifetime.start
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 0
// CHECK:   store %swift.type* %1, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 1
// CHECK:   store %swift.type* %2, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_ELT:%.*]] = getelementptr inbounds { %swift.type*, %swift.type*, %swift.type* }, { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]], i32 0, i32 2
// CHECK:   store %swift.type* %3, %swift.type** [[BUFFER_ELT]]
// CHECK:   [[BUFFER_PTR:%.*]] = bitcast { %swift.type*, %swift.type*, %swift.type* }* [[BUFFER]] to i8*
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @swift_getGenericMetadata([[INT]] %0, i8* [[BUFFER_PTR]], %swift.type_descriptor* {{.*}} @"$S17generic_metatypes9ThreeArgsVMn" {{.*}})
// CHECK:   [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$S17generic_metatypes8FourArgsVyAA3FooVAA3BarCAeGGMa"
// CHECK-SAME:    ([[INT]]) [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[BUFFER:%.*]] = alloca [4 x i8*]
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 255)
// CHECK:   [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:   call void @llvm.lifetime.start
// CHECK-NEXT: [[SLOT_0:%.*]] = getelementptr inbounds [4 x i8*], [4 x i8*]* [[BUFFER]], i32 0, i32 0
// CHECK-NEXT: store {{.*}}@"$S17generic_metatypes3FooVMf"{{.*}}, i8** [[SLOT_0]]
// CHECK-NEXT: [[SLOT_1:%.*]] = getelementptr inbounds [4 x i8*], [4 x i8*]* [[BUFFER]], i32 0, i32 1
// CHECK-NEXT: [[T0:%.*]] = bitcast %swift.type* [[BAR]] to i8*
// CHECK-NEXT: store i8* [[T0]], i8** [[SLOT_1]]
// CHECK-NEXT: [[SLOT_2:%.*]] = getelementptr inbounds [4 x i8*], [4 x i8*]* [[BUFFER]], i32 0, i32 2
// CHECK-NEXT: store {{.*}}@"$S17generic_metatypes3FooVMf"{{.*}}, i8** [[SLOT_2]]
// CHECK-NEXT: [[SLOT_3:%.*]] = getelementptr inbounds [4 x i8*], [4 x i8*]* [[BUFFER]], i32 0, i32 3
// CHECK-NEXT: [[T0:%.*]] = bitcast %swift.type* [[BAR]] to i8*
// CHECK-NEXT: store i8* [[T0]], i8** [[SLOT_3]]
// CHECK-NEXT: [[BUFFER_PTR:%.*]] = bitcast [4 x i8*]* [[BUFFER]] to i8**
// CHECK-NEXT: call swiftcc %swift.metadata_response @"$S17generic_metatypes8FourArgsVMa"([[INT]] %0, i8** [[BUFFER_PTR]]) [[NOUNWIND_ARGMEM:#[0-9]+]]
// CHECK: call void @llvm.lifetime.end.p0i8

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$S17generic_metatypes8FiveArgsVyAA3FooVAA3BarCAegEGMa"
// CHECK-SAME:    ([[INT]]) [[NOUNWIND_READNONE_OPT]]
// CHECK:   [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S17generic_metatypes3BarCMa"([[INT]] 255)
// CHECK:   [[BAR:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK:   call swiftcc %swift.metadata_response @"$S17generic_metatypes8FiveArgsVMa"([[INT]] %0, i8**

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S17generic_metatypes8FiveArgsVMa"
// CHECK-SAME:    ([[INT]], i8**) [[NOUNWIND_OPT:#[0-9]+]]
// CHECK-NOT: alloc
// CHECK:   call swiftcc %swift.metadata_response @swift_getGenericMetadata([[INT]] %0, i8* {{.*}}, %swift.type_descriptor* {{.*}} @"$S17generic_metatypes8FiveArgsVMn" {{.*}})
// CHECK-NOT: call void @llvm.lifetime.end
// CHECK:   ret %swift.metadata_response

// CHECK: attributes [[NOUNWIND_READNONE_OPT]] = { nounwind readnone "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
// CHECK: attributes [[NOUNWIND_OPT]] = { nounwind "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "target-cpu"
// CHECK: attributes [[NOUNWIND_READNONE]] = { nounwind readnone }
// CHECK: attributes [[NOUNWIND_ARGMEM]] = { inaccessiblemem_or_argmemonly nounwind }
