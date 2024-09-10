// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/non_resilient_protocol.swiftmodule -module-name=non_resilient_protocol %S/../Inputs/non_resilient_protocol.swift

// RUN: %target-swift-frontend -I %t -emit-ir %s -target %target-cpu-apple-macos14.0 | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE -check-prefix=CHECK-USAGE-BEFORE
// RUN: %target-swift-frontend -I %t -emit-ir %s -target %target-cpu-apple-macos15.0 | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE -check-prefix=CHECK-USAGE-AFTER

// REQUIRES: OS=macosx

import resilient_protocol
import non_resilient_protocol

func acceptResilientSendableBase<T: ResilientSendableBase>(_: T.Type) { }

// CHECK-USAGE: define{{.*}}swiftcc void @"$s28protocol_resilience_sendable25passResilientSendableBaseyyF"()
func passResilientSendableBase() {
  // CHECK-USAGE-NOT: ret
  // CHECK-USAGE: [[METATYPE:%.*]] = extractvalue
  // CHECK-USAGE-BEFORE: [[WITNESS_TABLE:%.*]] = call ptr @"$s18resilient_protocol27ConformsToResilientSendableVAcA0eF4BaseAAWl"()
  // CHECK-USAGE-BEFORE-NEXT: call swiftcc void @"$s28protocol_resilience_sendable27acceptResilientSendableBaseyyxm010resilient_A00efG0RzlF"(ptr [[METATYPE]], ptr [[METATYPE]], ptr [[WITNESS_TABLE]])
  // CHECK-USAGE-AFTER-NEXT: call swiftcc void @"$s28protocol_resilience_sendable27acceptResilientSendableBaseyyxm010resilient_A00efG0RzlF"(ptr [[METATYPE]], ptr [[METATYPE]], ptr @"$s18resilient_protocol27ConformsToResilientSendableVAA0eF4BaseAAWP")
  acceptResilientSendableBase(ConformsToResilientSendable.self)
}

func acceptNonResilientSendableBase<T: NonResilientSendableBase>(_: T.Type) { }

// CHECK-USAGE: define{{.*}}swiftcc void @"$s28protocol_resilience_sendable28passNonResilientSendableBaseyyF"()
func passNonResilientSendableBase() {
  // CHECK-USAGE-NOT: ret
  // CHECK-USAGE: call swiftcc void @"$s28protocol_resilience_sendable30acceptNonResilientSendableBaseyyxm014non_resilient_A00efgH0RzlF"(ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVN", ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVN", ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVAA0fgH4BaseAAWP")
  acceptNonResilientSendableBase(ConformsToNonResilientSendable.self)
}
