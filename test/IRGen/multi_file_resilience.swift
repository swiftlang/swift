// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift

// RUN: %target-swift-frontend %use_no_opaque_pointers -module-name main -I %t -emit-ir -primary-file %s %S/Inputs/OtherModule.swift | %FileCheck %s -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -module-name main -I %t -emit-ir -primary-file %s %S/Inputs/OtherModule.swift

// Check that we correctly handle resilience when parsing as SIL + SIB.
// RUN: %target-swift-frontend -emit-sib -module-name main %S/Inputs/OtherModule.swift -I %t -o %t/other.sib
// RUN: %target-swift-frontend -emit-silgen -module-name main -primary-file %s %S/Inputs/OtherModule.swift -I %t -o %t/main.sil
// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-ir -module-name main -primary-file %t/main.sil %t/other.sib -I %t | %FileCheck %s -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -emit-ir -module-name main -primary-file %t/main.sil %t/other.sib -I %t

// This is a single-module version of the test case in
// multi_module_resilience.
// rdar://39763787

// CHECK-LABEL: define {{(dllexport |protected )?}}swiftcc void @"$s4main7copyFoo3fooAA0C0VAE_tF"
// CHECK: [[ARG:%.*]] = bitcast %swift.opaque* %0 to %T4main3FooV
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$s4main3FooVMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[VWT:%.*]] = load i8**,
//   Allocate 'copy'.
// CHECK: [[VWT_CAST:%.*]] = bitcast i8** [[VWT]] to %swift.vwtable*
// CHECK: [[SIZE_ADDR:%.*]] = getelementptr inbounds %swift.vwtable, %swift.vwtable* [[VWT_CAST]], i32 0, i32 8
// CHECK: [[SIZE:%.*]] = load [[INT]], [[INT]]* [[SIZE_ADDR]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, [[INT]] [[SIZE]],
// CHECK: [[COPY:%.*]] = bitcast i8* [[ALLOCA]] to [[FOO:%T4main3FooV]]*
//   Perform 'initializeWithCopy' via the VWT instead of trying to inline it.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[COPYFN:%.*]] = bitcast i8* [[T1]] to %swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)*
// CHECK: [[DEST:%.*]] = bitcast [[FOO]]* [[COPY]] to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[FOO]]* %1 to %swift.opaque*
// CHECK: call %swift.opaque* [[COPYFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
//   Perform 'initializeWithCopy' via the VWT.
// CHECK: [[DEST:%.*]] = bitcast [[FOO]]* [[ARG]] to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[FOO]]* [[COPY]] to %swift.opaque*
// CHECK: call %swift.opaque* [[COPYFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
public func copyFoo(foo: Foo) -> Foo {
  let copy = foo
  return copy
}
