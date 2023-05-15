// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-sil %s | %FileCheck -check-prefix=SIL %s

// This test makes sure that in various situations (ignoring errors), we
// properly handle deinits with move only types.

@_moveOnly
public struct FD {
    var i = 5
}

public func borrowVal(_ e : borrowing FD) {}
public func consumeVal(_ s: __owned FD) {}

// CHECK: sil hidden [ossa] @$s22moveonly_deinit_access0B17AccessConsumeTestyyAA2FDVzF : $@convention(thin) (@inout FD) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[MARK:%.*]] = mark_must_check [consumable_and_assignable] [[ARG]]
// CHECK:   [[ACCESS:%.*]] = begin_access [deinit] [unknown] [[MARK]]
// CHECK:   [[TAKE:%.*]] = load [take] [[ACCESS]]
// CHECK:   apply {{%.*}}([[TAKE]]) : $@convention(thin) (@owned FD) -> ()
// CHECK:   end_access [[ACCESS]]
// CHECK: } // end sil function '$s22moveonly_deinit_access0B17AccessConsumeTestyyAA2FDVzF'
//
// SIL: sil hidden @$s22moveonly_deinit_access0B17AccessConsumeTestyyAA2FDVzF : $@convention(thin) (@inout FD) -> () {
// SIL:   begin_access [deinit] [static]
// SIL: } // end sil function '$s22moveonly_deinit_access0B17AccessConsumeTestyyAA2FDVzF'
func deinitAccessConsumeTest(_ x: inout FD) {
    consumeVal(x)
    x = FD()
}

