// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s %S/Inputs/vtable_multi_file_helper.swift -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: define hidden swiftcc void @"$s17vtable_multi_file36baseClassVtablesIncludeImplicitInitsyyF"() {{.*}} {
func baseClassVtablesIncludeImplicitInits() {
  // CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$s17vtable_multi_file8SubclassCMa"(i64 0)
  // CHECK: [[T0:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to { i64, %swift.bridge* } (%swift.type*)**
  // CHECK: [[T2:%.*]] = getelementptr inbounds { i64, %swift.bridge* } (%swift.type*)*, { i64, %swift.bridge* } (%swift.type*)** [[T1]], i64 11
  // CHECK: load { i64, %swift.bridge* } (%swift.type*)*, { i64, %swift.bridge* } (%swift.type*)** [[T2]]
  markUsed(Subclass.classProp)
}
