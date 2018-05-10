// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s %S/Inputs/vtable_multi_file_helper.swift -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: define hidden swiftcc void @"$S17vtable_multi_file36baseClassVtablesIncludeImplicitInitsyyF"() {{.*}} {
func baseClassVtablesIncludeImplicitInits() {
  // CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$S17vtable_multi_file8SubclassCMa"(i64 0)
  // CHECK: [[T0:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to { %swift.bridge*, i64 } (%swift.type*)**
  // CHECK: [[T2:%.*]] = getelementptr inbounds { %swift.bridge*, i64 } (%swift.type*)*, { %swift.bridge*, i64 } (%swift.type*)** [[T1]], i64 11
  // CHECK: load { %swift.bridge*, i64 } (%swift.type*)*, { %swift.bridge*, i64 } (%swift.type*)** [[T2]]
  markUsed(Subclass.classProp)
}
