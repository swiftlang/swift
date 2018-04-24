// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/generic_vtable.swift
// RUN: %target-swift-frontend %t/generic_vtable.swift -emit-ir | %FileCheck %t/generic_vtable.swift --check-prefix=CHECK

// REQUIRES: CPU=x86_64

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


//// Nominal type descriptor for 'Base' does not have any method descriptors.

// CHECK-LABEL: @"$S14generic_vtable4BaseCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, reflectable, is class, is unique
// CHECK-SAME: <i32 0x8004_0050>,
// -- vtable offset
// CHECK-SAME: i32 10,
// -- vtable size
// CHECK-SAME: i32 3
// -- no method descriptors -- class is fully concrete
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
// CHECK-SAME: , align 8


//// Nominal type descriptor for 'Derived' has method descriptors.

// CHECK-LABEL: @"$S14generic_vtable7DerivedCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, reflectable, is class, is unique, is generic
// CHECK-SAME: <i32 0x8004_00D0>,
// -- vtable offset
// CHECK-SAME: i32 14,
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
// CHECK-SAME: }>, align 8


//// Nominal type descriptor for 'Concrete' has method descriptors.

// CHECK-LABEL: @"$S14generic_vtable8ConcreteCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, reflectable, is class, is unique
// CHECK-SAME: <i32 0x8004_0050>,
// -- vtable offset
// CHECK-SAME: i32 15,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m4()
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteC2m4yyF"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata for 'Concrete' does not have any vtable entries; the vtable is
//// filled in at initialization time.

// CHECK-LABEL: @"$S14generic_vtable8ConcreteCMf" = internal global <{{.*}}> <{
// -- nominal type descriptor
// CHECK-SAME: @"$S14generic_vtable8ConcreteCMn",
// -- ivar destroyer
// CHECK-SAME: i8* null
// --
// CHECK-SAME: }>, align 8


//// Metadata initialization function for 'Derived' copies superclass vtable
//// and installs overrides for 'm2()' and 'init()'.

// CHECK-LABEL: define internal %swift.type* @"$S14generic_vtable7DerivedCMi"(%swift.type_descriptor*, i8**, i8**)

// - 2 immediate members:
//   - type metadata for generic parameter T,
//   - and vtable entry for 'm3()'
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** %1, i8** %2)
// CHECK: ret %swift.type* [[METADATA]]

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S14generic_vtable7DerivedCMr"
// CHECK-SAME:    (%swift.type* [[METADATA:%.*]], i8*, i8**) {{.*}} {
// CHECK: call void @swift_initClassMetadata(%swift.type* [[METADATA]], i64 0, {{.*}})

// -- method override for 'm2()'
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i64 11
// CHECK: store i8* bitcast (void (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedC2m2yyF" to i8*), i8** [[VTABLE0]], align 8

// -- method override for 'init()'
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// CHECK: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i64 12
// CHECK: store i8* bitcast (%T14generic_vtable7DerivedC* (%T14generic_vtable7DerivedC*)* @"$S14generic_vtable7DerivedCACyxGycfc" to i8*), i8** [[VTABLE1]], align 8

// CHECK: ret %swift.metadata_response


//// Metadata initialization function for 'Concrete' copies superclass vtable
//// and installs overrides for 'init()' and 'm3()'.

// CHECK-LABEL: define private void @initialize_metadata_Concrete(i8*)
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14generic_vtable7DerivedCySiGMa"(i64 1)
// CHECK: [[SUPERCLASS:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: store %swift.type* [[SUPERCLASS]], %swift.type** getelementptr inbounds {{.*}} @"$S14generic_vtable8ConcreteCMf"
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_relocateClassMetadata({{.*}}, i64 96, i64 1)
// CHECK: call void @swift_initClassMetadata(%swift.type* [[METADATA]], i64 0, {{.*}})

// -- method override for 'init()'
// CHECK: store i8* bitcast (%T14generic_vtable8ConcreteC* (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteCACycfc" to i8*), i8**

// -- method override for 'm3()'
// CHECK: store i8* bitcast (void (%T14generic_vtable8ConcreteC*)* @"$S14generic_vtable8ConcreteC2m3yyF" to i8*), i8**

// CHECK: store atomic %swift.type* [[METADATA]], %swift.type** @"$S14generic_vtable8ConcreteCML" release, align 8
// CHECK: ret void
