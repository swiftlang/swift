// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s %S/Inputs/vtable_multi_file_helper.swift -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: define hidden swiftcc void @_T017vtable_multi_file36baseClassVtablesIncludeImplicitInitsyyF() {{.*}} {
func baseClassVtablesIncludeImplicitInits() {
  // CHECK: [[T0:%.*]] = call %swift.type* @_T017vtable_multi_file8SubclassCMa()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to { i64, i64, i64 } (%swift.type*)**
  // CHECK: [[T2:%.*]] = getelementptr inbounds { i64, i64, i64 } (%swift.type*)*, { i64, i64, i64 } (%swift.type*)** [[T1]], i64 11
  // CHECK: load { i64, i64, i64 } (%swift.type*)*, { i64, i64, i64 } (%swift.type*)** [[T2]]
  markUsed(Subclass.classProp)
}
