// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience %s | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -O %s

// CHECK: %swift.type = type { [[INT:i32|i64]] }

import resilient_protocol

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks26callResilientWitnessMethodyyx010resilient_A00E12BaseProtocolRzlF"(%swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.ResilientBaseProtocol)
// CHECK: call swiftcc [[INT]] @"$S18resilient_protocol21ResilientBaseProtocolP11requirementSiyFTj"(%swift.opaque* noalias nocapture swiftself %0, %swift.type* %T, i8** %T.ResilientBaseProtocol)
// CHECK: ret void
public func callResilientWitnessMethod<T : ResilientBaseProtocol>(_ value: T) {
  _ = value.requirement()
}

public protocol MyResilientProtocol {
  func returnsVoid(x: Bool)

  func returnsBool() -> Bool
  func returnsAny() -> Any

  func throwingFunc() throws

  func genericFunc<T>(_: T) -> T

  var property: Bool { get set }
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks19MyResilientProtocolP11returnsVoid1xySb_tFTj"(i1, %swift.opaque* noalias nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_GEP:%.*]] = getelementptr inbounds i8*, i8** %3, i32 1
// CHECK:      [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_GEP]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to void (i1, %swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: call swiftcc void [[FN]](i1 %0, %swift.opaque* noalias nocapture swiftself %1, %swift.type* %2, i8** %3)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i1 @"$S26protocol_resilience_thunks19MyResilientProtocolP11returnsBoolSbyFTj"(%swift.opaque* noalias nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %2, i32 2
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to i1 (%swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: [[RESULT:%.*]] = call swiftcc i1 [[FN]](%swift.opaque* noalias nocapture swiftself %0, %swift.type* %1, i8** %2)
// CHECK-NEXT: ret i1 [[RESULT]]

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks19MyResilientProtocolP10returnsAnyypyFTj"(%Any* noalias nocapture sret, %swift.opaque* noalias nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %3, i32 3
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to void (%Any*, %swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: call swiftcc void [[FN]](%Any* noalias nocapture sret %0, %swift.opaque* noalias nocapture swiftself %1, %swift.type* %2, i8** %3)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks19MyResilientProtocolP12throwingFuncyyKFTj"(%swift.opaque* noalias nocapture swiftself, %swift.error**{{( swifterror)?}}, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %3, i32 4
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.error**, %swift.type*, i8**)*
// CHECK-NEXT: call swiftcc void [[FN]](%swift.opaque* noalias nocapture swiftself %0, %swift.error**{{( swifterror)?}} %1, %swift.type* %2, i8** %3)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks19MyResilientProtocolP11genericFuncyqd__qd__lFTj"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type*, %swift.opaque* noalias nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %5, i32 5
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to void (%swift.opaque*, %swift.opaque*, %swift.type*, %swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: call swiftcc void [[FN]](%swift.opaque* noalias nocapture sret %0, %swift.opaque* noalias nocapture %1, %swift.type* %2, %swift.opaque* noalias nocapture swiftself %3, %swift.type* %4, i8** %5)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i1 @"$S26protocol_resilience_thunks19MyResilientProtocolP8propertySbvgTj"(%swift.opaque* noalias nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %2, i32 6
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to i1 (%swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: [[RESULT:%.*]] = call swiftcc i1 %5(%swift.opaque* noalias nocapture swiftself %0, %swift.type* %1, i8** %2)
// CHECK-NEXT: ret i1 [[RESULT]]

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S26protocol_resilience_thunks19MyResilientProtocolP8propertySbvsTj"(i1, %swift.opaque* nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %3, i32 7
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to void (i1, %swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: call swiftcc void [[FN]](i1 %0, %swift.opaque* nocapture swiftself %1, %swift.type* %2, i8** %3)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc { i8*, {{i32|i64}} } @"$S26protocol_resilience_thunks19MyResilientProtocolP8propertySbvmTj"(i8*, [{{12|24}} x i8]* nocapture dereferenceable({{12|24}}), %swift.opaque* nocapture swiftself, %swift.type*, i8**)
// CHECK:      [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %4, i32 8
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[WITNESS]] to { i8*, [[INT]] } (i8*, [{{12|24}} x i8]*, %swift.opaque*, %swift.type*, i8**)*
// CHECK-NEXT: [[RESULT:%.*]] = call swiftcc { i8*, [[INT]] } [[FN]](i8* %0, [{{12|24}} x i8]* nocapture dereferenceable({{12|24}}) %1, %swift.opaque* nocapture swiftself %2, %swift.type* %3, i8** %4)
// CHECK-NEXT: ret { i8*, [[INT]] } [[RESULT]]
