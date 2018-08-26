// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/generic_vtable.swift
// RUN: %target-swift-frontend %t/generic_vtable.swift -emit-ir | %FileCheck %t/generic_vtable.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize

public class Base {
  public func m1() {}
  public func m2() {}
}

public class Derived<T> : Base {
  public override func m2() {}
  public func m3() {}
}

public class Concrete : Derived<Int> {
  public override func m3() {}
  public func m4() {}
}


//// Nominal type descriptor for 'Base' with method descriptors.

// CHECK-LABEL: @"$S14generic_vtable4BaseCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, is class, is unique
// CHECK-SAME: <i32 0x8000_0050>,
// -- vtable offset
// CHECK-32-SAME: i32 16,
// CHECK-64-SAME: i32 10,
// -- vtable size
// CHECK-SAME: i32 3,
// -- vtable entry for m1()
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseC2m1yyF"
// -- vtable entry for m2()
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseC2m2yyF"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata for 'Base' has a static vtable.

// CHECK-LABEL: @"$S14generic_vtable4BaseCMf" = internal global
// -- vtable entry for 'm1()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseC2m1yyF"
// -- vtable entry for 'm2()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseC2m2yyF"
// -- vtable entry for 'init()'
// CHECK-SAME: %T14generic_vtable4BaseC* (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseCACycfc"
// --
// CHECK-SAME: , align


//// Nominal type descriptor for 'Derived' with method descriptors.

// CHECK-LABEL: @"$S14generic_vtable7DerivedCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, is class, is unique, is generic
// CHECK-SAME: <i32 0x8000_00D0>,
// -- vtable offset
// CHECK-32-SAME: i32 17,
// CHECK-64-SAME: i32 14,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m3()
// CHECK-SAME: void (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedC2m3yyF"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata pattern for 'Derived' has an empty vtable, filled in at
//// instantiation time.

// CHECK-LABEL: @"$S14generic_vtable7DerivedCMP" = internal constant <{{.*}}> <{
// -- ivar destroyer
// CHECK-SAME: i32 0
// --
// CHECK-SAME: }>, align


//// Nominal type descriptor for 'Concrete' with method descriptors.

// CHECK-LABEL: @"$S14generic_vtable8ConcreteCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, in-place initialization, is class, is unique
// CHECK-SAME: <i32 0x8001_0050>,
// -- vtable offset
// CHECK-32-SAME: i32 19,
// CHECK-64-SAME: i32 15,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m4()
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteC2m4yyF"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata for 'Concrete' has a static vtable.

// CHECK-LABEL: @"$S14generic_vtable8ConcreteCMf" = internal global <{{.*}}> <{
// -- nominal type descriptor
// CHECK-SAME: @"$S14generic_vtable8ConcreteCMn",
// -- vtable entry for 'm1()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$S14generic_vtable4BaseC2m1yyF"
// -- vtable entry for 'm2()'
// CHECK-SAME: void (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedC2m2yyF"
// -- vtable entry for 'init()'
// CHECK-SAME: %T14generic_vtable8ConcreteC* (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteCACycfc"
// -- vtable entry for 'm3()'
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteC2m3yyF"
// -- vtable entry for 'm4()'
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteC2m4yyF"
// --
// CHECK-SAME: }>, align


//// Metadata initialization function for 'Derived' copies superclass vtable
//// and installs overrides for 'm2()' and 'init()'.

// CHECK-LABEL: define internal %swift.type* @"$S14generic_vtable7DerivedCMi"(%swift.type_descriptor*, i8**, i8*)

// - 2 immediate members:
//   - type metadata for generic parameter T,
//   - and vtable entry for 'm3()'
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** %1, i8* %2)
// CHECK: ret %swift.type* [[METADATA]]

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S14generic_vtable7DerivedCMr"
// CHECK-SAME:    (%swift.type* [[METADATA:%.*]], i8*, i8**) {{.*}} {
// CHECK: call void @swift_initClassMetadata(%swift.type* [[METADATA]], %swift.type* {{%.*}}, [[INT]] 0, {{.*}})

// -- method override for 'm2()'
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK-32: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], [[INT]] 14
// CHECK-64: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], [[INT]] 11
// CHECK: store i8* bitcast (void (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedC2m2yyF" to i8*), i8** [[VTABLE0]]

// -- method override for 'init()'
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK-32: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], [[INT]] 15
// CHECK-64: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], [[INT]] 12
// CHECK: store i8* bitcast (%T14generic_vtable7DerivedC* (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedCACyxGycfc" to i8*), i8** [[VTABLE1]]

// CHECK: ret %swift.metadata_response


// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %swift.metadata_response @"$S14generic_vtable8ConcreteCMa"
// CHECK: call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S14generic_vtable8ConcreteCMn" to {{.*}}))
// CHECK: ret

//// Metadata response function for 'Concrete' sets the superclass.

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S14generic_vtable8ConcreteCMr"(%swift.type*, i8*, i8**)
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14generic_vtable7DerivedCySiGMa"([[INT]] 257)
// CHECK-NEXT: [[SUPERCLASS:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT: [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 1
// CHECK-NEXT: br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// -- ClassLayoutFlags is 256 / 0x100, HasStaticVTable
// CHECK: call void @swift_initClassMetadata(%swift.type* %0, %swift.type* [[SUPERCLASS]], [[INT]] 256, {{.*}})
// CHECK: br label %metadata-dependencies.cont

// CHECK: metadata-dependencies.cont:
// CHECK: ret %swift.metadata_response
