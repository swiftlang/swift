// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-parameterized-existential-types -emit-module %S/Inputs/parameterized_protocol_other.swift -emit-module-path %t/parameterized_protocol_other.swiftmodule
// RUN: %target-typecheck-verify-swift -enable-parameterized-existential-types -I%t

import parameterized_protocol_other

func testParameterizedProtocol(_: some MySequence<Int>) {}
func testParameterizedProtocol(x : any MySequence<Int>) -> MySequenceHolder<Int> {
  return MySequenceHolder(seq: x)
}
