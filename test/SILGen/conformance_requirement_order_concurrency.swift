// RUN: %target-swift-emit-silgen %s -module-name main | %FileCheck %s

// See conformance_requirement_order.swift for the general case.
//
// For the _Concurrency module, the mistake is baked into the ABI. For example,
// we mangle the below type as Swift.Executor, but order it as if it were
// _Concurrency.Executor.

// CHECK-LABEL: sil hidden [ossa] @$s4main27requirementOrderConcurrencyyyxSTRzScFRzlF : $@convention(thin) <T where T : Sequence, T : Executor> (@guaranteed T) -> () {
func requirementOrderConcurrency<T: Executor & Sequence>(_: T) {}