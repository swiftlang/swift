// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s %S/Inputs/vtable_multi_file_helper.swift -emit-ir | FileCheck %s

// CHECK-LABEL: define void @_TF17vtable_multi_file36baseClassVtablesIncludeImplicitInitsFT_T_() {
func baseClassVtablesIncludeImplicitInits() {
  // CHECK: [[T0:%.*]] = call %swift.type* @_TMaC17vtable_multi_file8Subclass()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to { i8*, i64, i64 } (%swift.type*)**
  // CHECK: [[T2:%.*]] = getelementptr inbounds { i8*, i64, i64 } (%swift.type*)** [[T1]], i64 10
  // CHECK: load { i8*, i64, i64 } (%swift.type*)** [[T2]]
  println(Subclass.classProp)
}
