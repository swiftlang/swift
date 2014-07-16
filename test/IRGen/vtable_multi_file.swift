// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s %S/Inputs/vtable_multi_file_helper.swift -emit-ir | FileCheck %s

// CHECK-LABEL: define void @_TF17vtable_multi_file36baseClassVtablesIncludeImplicitInitsFT_T_() {
func baseClassVtablesIncludeImplicitInits() {
  // CHECK: = load { i8*, i64, i64 } (%swift.type*)** getelementptr inbounds ({ i8*, i64, i64 } (%swift.type*)** bitcast (%swift.type* getelementptr inbounds (%swift.full_heapmetadata* @_TMdC17vtable_multi_file8Subclass, i32 0, i32 2) to { i8*, i64, i64 } (%swift.type*)**), i64 9)
  println(Subclass.classProp)
}
