// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s --check-prefix=CHECK

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

// CHECK-LABEL: @_T014generic_vtable4BaseCMn = {{(protected )?}}constant
// -- nesting depth
// CHECK-SAME: i16 1,
// -- flags: has vtable
// CHECK-SAME: i16 4,
// -- generic parameters at depth 0
// CHECK-SAME: i32 0,
// -- vtable offset
// CHECK-SAME: i32 10,
// -- vtable size
// CHECK-SAME: i32 3
// -- no method descriptors -- class is fully concrete
// CHECK-SAME: section "{{.*}}", align 8

//// Type metadata for 'Base' has a static vtable.

// CHECK-LABEL: @_T014generic_vtable4BaseCMf = internal global
// -- vtable entry for 'm1()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @_T014generic_vtable4BaseC2m1yyF
// -- vtable entry for 'm2()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @_T014generic_vtable4BaseC2m2yyF
// -- vtable entry for 'init()'
// CHECK-SAME: %T14generic_vtable4BaseC* (%T14generic_vtable4BaseC*)* @_T014generic_vtable4BaseCACycfc
// --
// CHECK-SAME: , align 8


//// Nominal type descriptor for 'Derived' has method descriptors.

// CHECK-LABEL: @_T014generic_vtable7DerivedCMn = {{(protected )?}}constant
// -- nesting depth
// CHECK-SAME: i16 1,
// -- flags: has vtable
// CHECK-SAME: i16 4,
// -- generic parameters at depth 0
// CHECK-SAME: i32 1,
// -- vtable offset
// CHECK-SAME: i32 14,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m3()
// CHECK-SAME: void (%T14generic_vtable7DerivedC*)* @_T014generic_vtable7DerivedC2m3yyF
// --
// CHECK-SAME: section "{{.*}}", align 8

//// Type metadata pattern for 'Derived' has an empty vtable, filled in at
//// instantiation time.

// CHECK-LABEL: @_T014generic_vtable7DerivedCMP = internal global
// -- vtable entry for 'm1()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'm2()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'init()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'm3()'
// CHECK-SAME: i8* null
// --
// CHECK-SAME: , align 8


//// Nominal type descriptor for 'Concrete' has method descriptors.

// CHECK-LABEL: @_T014generic_vtable8ConcreteCMn = {{(protected )?}}constant
// -- nesting depth
// CHECK-SAME: i16 1,
// -- flags: has vtable
// CHECK-SAME: i16 4,
// -- generic parameters at depth 0
// CHECK-SAME: i32 0,
// -- vtable offset
// CHECK-SAME: i32 15,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m4()
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @_T014generic_vtable8ConcreteC2m4yyF
// --
// CHECK-SAME: section "{{.*}}", align 8

//// Type metadata for 'Concrete' has an empty vtable, filled in at
//// initialization time.

// CHECK-LABEL: @_T014generic_vtable8ConcreteCMf = internal global
// -- vtable entry for 'm1()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'm2()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'init()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'm3()'
// CHECK-SAME: i8* null,
// -- vtable entry for 'm4()'
// CHECK-SAME: i8* null
// --
// CHECK-SAME: , align 8


//// Metadata initialization function for 'Derived' copies superclass vtable
//// and installs overrides for 'm2()' and 'init()'.

// CHECK-LABEL: define private %swift.type* @create_generic_metadata_Derived(%swift.type_pattern*, i8**)
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata({{.*}})
// CHECK: [[METADATA2:%.*]] = call %swift.type* @swift_initClassMetadata_UniversalStrategy({{.*}})
// -- method override for 'm2()'
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA2]] to i8**
// CHECK: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 11
// CHECK: store i8* bitcast (void (%T14generic_vtable7DerivedC*)* @_T014generic_vtable7DerivedC2m2yyF to i8*), i8** [[VTABLE0]], align 8
// -- method override for 'init()'
// CHECK: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 12
// CHECK: store i8* bitcast (%T14generic_vtable7DerivedC* (%T14generic_vtable7DerivedC*)* @_T014generic_vtable7DerivedCACyxGycfc to i8*), i8** [[VTABLE1]], align 8
// CHECK: ret %swift.type* [[METADATA]]


//// Metadata initialization function for 'Concrete' copies superclass vtable
//// and installs overrides for 'init()' and 'm3()'.

// CHECK-LABEL: define private void @initialize_metadata_Concrete(i8*)
// CHECK: [[SUPERCLASS:%.*]] = call %swift.type* @_T014generic_vtable7DerivedCySiGMa()
// CHECK: store %swift.type* [[SUPERCLASS]], %swift.type** getelementptr inbounds {{.*}} @_T014generic_vtable8ConcreteCMf
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_initClassMetadata_UniversalStrategy({{.*}})
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA]] to i8**
// -- method override for 'init()'
// CHECK: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 12
// CHECK: store i8* bitcast (%T14generic_vtable8ConcreteC* (%T14generic_vtable8ConcreteC*)* @_T014generic_vtable8ConcreteCACycfc to i8*), i8** [[VTABLE0]], align 8
// -- method override for 'm3()'
// CHECK: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 14
// CHECK: store i8* bitcast (void (%T14generic_vtable8ConcreteC*)* @_T014generic_vtable8ConcreteC2m3yyF to i8*), i8** [[VTABLE1]], align 8
// CHECK: ret void
