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

// CHECK-LABEL: @_T014generic_vtable4BaseCMn = {{(protected )?}}constant
// -- flags: has vtable
// CHECK-SAME: i32 4,
// -- vtable offset
// CHECK-SAME: i32 10,
// -- vtable size
// CHECK-SAME: i32 3
// --
// CHECK-SAME: section "{{.*}}", align 8

// CHECK-LABEL: @_T014generic_vtable7DerivedCMn = {{(protected )?}}constant
// -- flags: has vtable
// CHECK-SAME: i32 4,
// -- vtable offset
// CHECK-SAME: i32 14,
// -- vtable size
// CHECK-SAME: i32 1
// --
// CHECK-SAME: section "{{.*}}", align 8

// CHECK-LABEL: define private %swift.type* @create_generic_metadata_Derived(%swift.type_pattern*, i8**)
// CHECK: [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata({{.*}})
// CHECK: [[METADATA2:%.*]] = call %swift.type* @swift_initClassMetadata_UniversalStrategy({{.*}})
// CHECK: [[WORDS:%.*]] = bitcast %swift.type* [[METADATA2]] to i8**
// CHECK: [[VTABLE0:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 11
// CHECK: store i8* bitcast (void (%T14generic_vtable7DerivedC*)* @_T014generic_vtable7DerivedC2m2yyF to i8*), i8** [[VTABLE0]], align 8
// CHECK: [[VTABLE1:%.*]] = getelementptr inbounds i8*, i8** [[WORDS]], i32 12
// CHECK: store i8* bitcast (%T14generic_vtable7DerivedC* (%T14generic_vtable7DerivedC*)* @_T014generic_vtable7DerivedCACyxGycfc to i8*), i8** [[VTABLE1]], align 8
// CHECK: ret %swift.type* [[METADATA]]
