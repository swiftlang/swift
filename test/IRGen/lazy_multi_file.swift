// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s %S/Inputs/lazy_multi_file_helper.swift -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386 || CPU=x86_64

// CHECK: %T15lazy_multi_file8SubclassC = type <{ %swift.refcounted, %[[OPTIONAL_INT_TY:TSiSg]], [{{[0-9]+}} x i8], %TSS }>
// CHECK: %[[OPTIONAL_INT_TY]] = type <{ [{{[0-9]+}} x i8], [1 x i8] }>
// CHECK: %T15lazy_multi_file13LazyContainerV = type <{ %[[OPTIONAL_INT_TY]] }>

class Subclass : LazyContainerClass {
  final var str = "abc"

  // FIXME(TODO: JIRA): i386 String grew beyond 3 words, so we have to allow
  // an indirect return value. When it shrinks back, remove the optional
  // indirect out.
  //
  // CHECK-LABEL: @"$s15lazy_multi_file8SubclassC6getStrSSyF"({{(\%TSS\* noalias nocapture sret, )?}}%T15lazy_multi_file8SubclassC* swiftself) {{.*}} {
  func getStr() -> String {
    // CHECK: = getelementptr inbounds %T15lazy_multi_file8SubclassC, %T15lazy_multi_file8SubclassC* %0, i32 0, i32 3
    return str
  }
}

// CHECK-LABEL: @"$s15lazy_multi_file4testSiyF"() {{.*}} {
func test() -> Int {
  var container = LazyContainer()
  useLazyContainer(container)
  return container.lazyVar
}
