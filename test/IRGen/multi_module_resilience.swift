// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: %target-swift-frontend -emit-module -enable-resilience \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift

// RUN: %target-swift-frontend -emit-module -I %t \
// RUN:   -emit-module-path=%t/OtherModule.swiftmodule \
// RUN:   -module-name=OtherModule %S/Inputs/OtherModule.swift

// RUN: %target-swift-frontend -module-name main -I %t -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize

// rdar://39763787

import OtherModule

// CHECK-LABEL: define {{(dllexport |protected )?}}swiftcc void @"$S4main7copyFoo3foo11OtherModule0C0VAF_tF"
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S11OtherModule3FooVMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[VWT:%.*]] = load i8**,
//   Allocate 'copy'.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 8
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[SIZE:%.*]] = ptrtoint i8* [[T1]] to [[INT]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, [[INT]] [[SIZE]],
// CHECK: [[COPY:%.*]] = bitcast i8* [[ALLOCA]] to [[FOO:%T11OtherModule3FooV]]*
//   Perform 'initializeWithCopy' via the VWT instead of trying to inline it.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[COPYFN:%.*]] = bitcast i8* [[T1]] to %swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)*
// CHECK: [[DEST:%.*]] = bitcast [[FOO]]* [[COPY]] to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[FOO]]* %1 to %swift.opaque*
// CHECK: call %swift.opaque* [[COPYFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
//   Perform 'initializeWithTake' via the VWT.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[TAKEFN:%.*]] = bitcast i8* [[T1]] to %swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)*
// CHECK: [[DEST:%.*]] = bitcast [[FOO]]* %0 to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[FOO]]* [[COPY]] to %swift.opaque*
// CHECK: call %swift.opaque* [[TAKEFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
public func copyFoo(foo: Foo) -> Foo {
  let copy = foo
  return copy
}

// CHECK-LABEL: define {{(dllexport |protected )?}}swiftcc void @"$S4main7copyBar3bar11OtherModule0C0VAF_tF"
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S11OtherModule3BarVMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[VWT:%.*]] = load i8**,
//   Allocate 'copy'.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 8
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[SIZE:%.*]] = ptrtoint i8* [[T1]] to [[INT]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, [[INT]] [[SIZE]],
// CHECK: [[COPY:%.*]] = bitcast i8* [[ALLOCA]] to [[BAR:%T11OtherModule3BarV]]*
//   Perform 'initializeWithCopy' via the VWT instead of trying to inline it.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[COPYFN:%.*]] = bitcast i8* [[T1]] to %swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)*
// CHECK: [[DEST:%.*]] = bitcast [[BAR]]* [[COPY]] to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[BAR]]* %1 to %swift.opaque*
// CHECK: call %swift.opaque* [[COPYFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
//   Perform 'initializeWithTake' via the VWT.
// CHECK: [[T0:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK: [[T1:%.*]] = load i8*, i8** [[T0]],
// CHECK: [[TAKEFN:%.*]] = bitcast i8* [[T1]] to %swift.opaque* (%swift.opaque*, %swift.opaque*, %swift.type*)*
// CHECK: [[DEST:%.*]] = bitcast [[BAR]]* %0 to %swift.opaque*
// CHECK: [[SRC:%.*]] = bitcast [[BAR]]* [[COPY]] to %swift.opaque*
// CHECK: call %swift.opaque* [[TAKEFN]](%swift.opaque* noalias [[DEST]], %swift.opaque* noalias [[SRC]], %swift.type* [[METADATA]])
public func copyBar(bar: Bar) -> Bar {
  let copy = bar
  return copy
}
