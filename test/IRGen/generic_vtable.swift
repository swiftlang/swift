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

// CHECK-LABEL: @"$s14generic_vtable4BaseCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, is class, is unique
// CHECK-SAME: <i32 0x8000_0050>,
// -- vtable offset
// CHECK-32-SAME: i32 16,
// CHECK-64-SAME: i32 10,
// -- vtable size
// CHECK-SAME: i32 3,
// -- vtable entry for m1()
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseC2m1yyF"
// -- vtable entry for m2()
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseC2m2yyF"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata for 'Base' has a static vtable.

// CHECK-LABEL: @"$s14generic_vtable4BaseCMf" = internal global
// -- destructor
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseCfD"
// -- value witness table
// CHECK-SAME: i8** @"$sBoWV"
// -- vtable entry for 'm1()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseC2m1yyF"
// -- vtable entry for 'm2()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseC2m2yyF"
// -- vtable entry for 'init()'
// CHECK-SAME: %T14generic_vtable4BaseC* (%swift.type*)* @"$s14generic_vtable4BaseCACycfC"
// --
// CHECK-SAME: , align


//// Nominal type descriptor for 'Derived' with method descriptors.

// CHECK-LABEL: @"$s14generic_vtable7DerivedCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, has override table, is class, is unique, is generic
// CHECK-SAME: <i32 0xC000_00D0>,
// -- vtable offset
// CHECK-32-SAME: i32 17,
// CHECK-64-SAME: i32 14,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m3()
// CHECK-SAME: void (%T14generic_vtable7DerivedC*)* @"$s14generic_vtable7DerivedC2m3yyF"
// -- override table size
// CHECK-SAME: i32 2,
// -- override for m2()
// CHECK-SAME: @"$s14generic_vtable4BaseCMn"
// CHECK-SAME: @"$s14generic_vtable4BaseCMn", i32 0, i32 14
// CHECK-SAME: @"$s14generic_vtable7DerivedC2m2yyF"
// -- override for constructor
// CHECK-SAME: @"$s14generic_vtable4BaseCMn"
// CHECK-SAME: @"$s14generic_vtable4BaseCMn", i32 0, i32 15
// CHECK-SAME: @"$s14generic_vtable7DerivedCACyxGycfC"
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata pattern for 'Derived' has an empty vtable, filled in at
//// instantiation time.

// CHECK-LABEL: @"$s14generic_vtable7DerivedCMP" = internal constant <{{.*}}> <{
// -- ivar destroyer
// CHECK-SAME: i32 0
// --
// CHECK-SAME: }>, align


//// Nominal type descriptor for 'Concrete' with method descriptors.

// CHECK-LABEL: @"$s14generic_vtable8ConcreteCMn" = {{(dllexport )?}}{{(protected )?}}constant
// -- flags: has vtable, has override table, in-place initialization, is class, is unique
// CHECK-SAME: <i32 0xC001_0050>,
// -- vtable offset
// CHECK-32-SAME: i32 19,
// CHECK-64-SAME: i32 15,
// -- vtable size
// CHECK-SAME: i32 1,
// -- vtable entry for m4()
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$s14generic_vtable8ConcreteC2m4yyF"
// -- override table size
// CHECK-SAME: i32 2,
// -- override for m3()
// CHECK-SAME: @"$s14generic_vtable7DerivedCMn"
// CHECK-SAME: @"$s14generic_vtable7DerivedCMn", i32 0, i32 23
// CHECK-SAME: @"$s14generic_vtable8ConcreteC2m3yyF"
// -- override for constructor
// CHECK-SAME: @"$s14generic_vtable4BaseCMn"
// CHECK-SAME: @"$s14generic_vtable4BaseCMn", i32 0, i32 15
// CHECK-SAME: @"$s14generic_vtable8ConcreteCACycfC"
// --
// CHECK-SAME: section "{{.*}}", align 4

//// Type metadata for 'Concrete' has a static vtable.

// CHECK-LABEL: @"$s14generic_vtable8ConcreteCMf" = internal global <{{.*}}> <{
// -- destructor
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$s14generic_vtable8ConcreteCfD",
// -- value witness table is filled in at runtime
// CHECK-SAME: i8** null,
// -- nominal type descriptor
// CHECK-SAME: @"$s14generic_vtable8ConcreteCMn",
// -- vtable entry for 'm1()'
// CHECK-SAME: void (%T14generic_vtable4BaseC*)* @"$s14generic_vtable4BaseC2m1yyF"
// -- vtable entry for 'm2()'
// CHECK-SAME: void (%T14generic_vtable7DerivedC*)* @"$s14generic_vtable7DerivedC2m2yyF"
// -- vtable entry for 'init()'
// CHECK-SAME: %T14generic_vtable8ConcreteC* (%swift.type*)* @"$s14generic_vtable8ConcreteCACycfC"
// -- vtable entry for 'm3()'
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$s14generic_vtable8ConcreteC2m3yyF"
// -- vtable entry for 'm4()'
// CHECK-SAME: void (%T14generic_vtable8ConcreteC*)* @"$s14generic_vtable8ConcreteC2m4yyF"
// --
// CHECK-SAME: }>, align


//// Method descriptors

// CHECK-LABEL: @"$s14generic_vtable4BaseC2m1yyFTq" ={{( dllexport)?}}{{( protected)?}} alias %swift.method_descriptor, getelementptr inbounds (<{{.*}}>* @"$s14generic_vtable4BaseCMn", i32 0, i32 13)
// CHECK-LABEL: @"$s14generic_vtable4BaseC2m2yyFTq" ={{( dllexport)?}}{{( protected)?}} alias %swift.method_descriptor, getelementptr inbounds (<{{.*}}* @"$s14generic_vtable4BaseCMn", i32 0, i32 14)
// CHECK-LABEL: @"$s14generic_vtable4BaseCACycfCTq" = hidden alias %swift.method_descriptor, getelementptr inbounds (<{{.*}}* @"$s14generic_vtable4BaseCMn", i32 0, i32 15)

// CHECK-LABEL: @"$s14generic_vtable7DerivedC2m3yyFTq" ={{( dllexport)?}}{{( protected)?}} alias %swift.method_descriptor, getelementptr inbounds (<{{.*}}>* @"$s14generic_vtable7DerivedCMn", i32 0, i32 23)

// CHECK-LABEL: @"$s14generic_vtable8ConcreteC2m4yyFTq" ={{( dllexport)?}}{{( protected)?}} alias %swift.method_descriptor, getelementptr inbounds (<{{.*}}>* @"$s14generic_vtable8ConcreteCMn", i32 0, i32 16)


//// Metadata initialization function for 'Derived' copies superclass vtable
//// and installs overrides for 'm2()' and 'init()'.

// CHECK-LABEL: define internal %swift.type* @"$s14generic_vtable7DerivedCMi"(%swift.type_descriptor*, i8**, i8*)

// - 2 immediate members:
//   - type metadata for generic parameter T,
//   - and vtable entry for 'm3()'
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_descriptor* %0, i8** %1, i8* %2)
// CHECK: ret %swift.type* [[METADATA]]

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$s14generic_vtable7DerivedCMr"
// CHECK-SAME:    (%swift.type* [[METADATA:%.*]], i8*, i8**) {{.*}} {
// CHECK: call void @swift_initClassMetadata(%swift.type* [[METADATA]], [[INT]] 0, {{.*}})

// CHECK: ret %swift.metadata_response


// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %swift.metadata_response @"$s14generic_vtable8ConcreteCMa"
// CHECK: call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$s14generic_vtable8ConcreteCMn" to {{.*}}))
// CHECK: ret

//// Metadata response function for 'Concrete' is fairly simple.

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$s14generic_vtable8ConcreteCMr"(%swift.type*, i8*, i8**)
// -- ClassLayoutFlags is 256 / 0x100, HasStaticVTable
// CHECK: call void @swift_initClassMetadata(%swift.type* %0, [[INT]] 256, {{.*}})
// CHECK: ret %swift.metadata_response
