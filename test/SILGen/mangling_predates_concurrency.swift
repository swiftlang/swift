// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// REQUIRES: concurrency


// CHECK: sil [ossa] @$s29mangling_predates_concurrency16excitingFunction5value4bodyyycx_yycSgtlF : $@convention(thin) <T where T : Sendable> (@in_guaranteed T, @guaranteed Optional<@Sendable @callee_guaranteed () -> ()>) -> @owned @callee_guaranteed () -> ()
@preconcurrency
public func excitingFunction<T: Sendable>(value: T, body: (@Sendable () -> Void)?) -> (@MainActor () -> Void) {
  { }
}

public protocol P { }

// CHECK: sil [ossa] @$s29mangling_predates_concurrency13lotsOfChangesyyXlSgyp_AA1P_pypXpAaD_XlXptF
@preconcurrency public func lotsOfChanges(
  _: Sendable, _: P & Sendable, _: Sendable.Type,
  _: (AnyObject & Sendable & P).Type
) -> (AnyObject & Sendable)? {
  nil
}
