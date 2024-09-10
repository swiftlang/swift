// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/non_resilient_protocol.swiftmodule -module-name=non_resilient_protocol %S/../Inputs/non_resilient_protocol.swift

// RUN: %target-swift-frontend -I %t -emit-ir %s -target %target-cpu-apple-macos14.0 | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE -check-prefix=CHECK-USAGE-BEFORE
// RUN: %target-swift-frontend -I %t -emit-ir %s -target %target-cpu-apple-macos15.0 | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE -check-prefix=CHECK-USAGE-AFTER

// REQUIRES: OS=macosx

import resilient_protocol
import non_resilient_protocol

@available(SwiftStdlib 5.10, *)
func acceptResilientSendableBase<T: ResilientSendableBase>(_: T.Type) { }

// CHECK-USAGE: define{{.*}}swiftcc void @"$s28protocol_resilience_sendable25passResilientSendableBaseyyF"()
@available(SwiftStdlib 5.10, *)
func passResilientSendableBase() {
  // CHECK-USAGE-NOT: ret
  // CHECK-USAGE: [[METATYPE:%.*]] = extractvalue
  // CHECK-USAGE: [[WITNESS_TABLE:%.*]] = call ptr @"$s18resilient_protocol27ConformsToResilientSendableVAcA0eF4BaseAAWl"()
  // CHECK-USAGE-NEXT: call swiftcc void @"$s28protocol_resilience_sendable27acceptResilientSendableBaseyyxm010resilient_A00efG0RzlF"(ptr [[METATYPE]], ptr [[METATYPE]], ptr [[WITNESS_TABLE]])
  acceptResilientSendableBase(ConformsToResilientSendable.self)
}

@available(SwiftStdlib 6.0, *)
func acceptNewResilientSendableBase<T: NewResilientSendableBase>(_: T.Type) { }

// CHECK-USAGE: define{{.*}}swiftcc void @"$s28protocol_resilience_sendable28passNewResilientSendableBaseyyF"()
@available(SwiftStdlib 6.0, *)
func passNewResilientSendableBase() {
  // CHECK-USAGE-NOT: ret
  // CHECK-USAGE: [[METATYPE:%.*]] = extractvalue
  // CHECK-USAGE-NEXT: call swiftcc void @"$s28protocol_resilience_sendable30acceptNewResilientSendableBaseyyxm010resilient_A00efgH0RzlF"(ptr [[METATYPE]], ptr [[METATYPE]], ptr @"$s18resilient_protocol30ConformsToNewResilientSendableVAA0efG4BaseAAWP")
  acceptNewResilientSendableBase(ConformsToNewResilientSendable.self)
}

@available(SwiftStdlib 5.10, *)
func acceptNonResilientSendableBase<T: NonResilientSendableBase>(_: T.Type) { }

// CHECK-USAGE: define{{.*}}swiftcc void @"$s28protocol_resilience_sendable28passNonResilientSendableBaseyyF"()
@available(SwiftStdlib 5.10, *)
func passNonResilientSendableBase() {
  // CHECK-USAGE-NOT: ret
  // CHECK-USAGE: call swiftcc void @"$s28protocol_resilience_sendable30acceptNonResilientSendableBaseyyxm014non_resilient_A00efgH0RzlF"(ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVN", ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVN", ptr @"$s22non_resilient_protocol30ConformsToNonResilientSendableVAA0fgH4BaseAAWP")
  acceptNonResilientSendableBase(ConformsToNonResilientSendable.self)
}
