// RUN: %target-swift-frontend -emit-sil -O -enable-sil-opaque-values -sil-verify-all %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting -o /dev/null

// REQUIRES: swift_feature_NoncopyableCasting

// After specialization this cast is from Optional<any P & ~Copyable> to the
// same type, which the cast optimizer folds into a move of the value. With
// opaque values, that move used to reach the "copy into" unreachable of the
// non-Copyable opaque value type lowering.

protocol P: ~Copyable {}
struct NC: P, ~Copyable {}

typealias O = Optional<any P & ~Copyable>

@inline(never)
func genericCaseLet<U: ~Copyable>(_ o: consuming O, _: U.Type) -> Bool {
  if case let v as U = o { _ = v; return true }
  _ = consume o
  return false
}

public func test() -> Bool {
  genericCaseLet(NC(), O.self)
}
