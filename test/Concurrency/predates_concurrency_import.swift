// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/StrictModule.swiftmodule -module-name StrictModule -warn-concurrency %S/Inputs/StrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/NonStrictModule.swiftmodule -module-name NonStrictModule %S/Inputs/NonStrictModule.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-typecheck-verify-swift -typecheck  -I %t

@_predatesConcurrency import NonStrictModule
@_predatesConcurrency import StrictModule
@_predatesConcurrency import OtherActors
// expected-remark@-1{{'@_predatesConcurrency' attribute on module 'OtherActors' is unused}}{{1-23=}}

func acceptSendable<T: Sendable>(_: T) { }

@available(SwiftStdlib 5.1, *)
func test(
  ss: StrictStruct, ns: NonStrictClass, oma: OtherModuleActor,
  ssc: SomeSendableClass
) async {
  acceptSendable(ss) // expected-warning{{type 'StrictStruct' does not conform to the 'Sendable' protocol}}
  acceptSendable(ns) // silence issue entirely
  acceptSendable(oma) // okay
  acceptSendable(ssc) // okay
}
