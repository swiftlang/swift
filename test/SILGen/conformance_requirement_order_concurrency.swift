// RUN: %target-swift-emit-silgen %s -module-name main | %FileCheck %s

// See conformance_requirement_order.swift for the general case.
//
// Make sure that protocols from the _Concurrency module are ordered using
// their real module name and not their ABI module name, which is "Swift".
//
// This was a mistake since they mangle like protocols from "Swift", but
// at this point we cannot break existing code.

// CHECK-LABEL: sil hidden [ossa] @$s4main27requirementOrderConcurrencyyyxSTRzScFRzlF : $@convention(thin) <T where T : Sequence, T : Executor> (@guaranteed T) -> () {
func requirementOrderConcurrency<T: Executor & Sequence>(_: T) {}