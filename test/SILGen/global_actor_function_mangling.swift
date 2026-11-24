// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -emit-silgen %s -module-name test | %FileCheck %s
// REQUIRES: concurrency

// Declarations don't mangle global actor types.
// CHECK: @$s4test10returnsOptyxycSgxy{{(Yb)?}}ScMYccSglF
func returnsOpt<R>(_ fn: (@MainActor () -> R)?) -> (() -> R)? {
  typealias Fn = (() -> R)?
  return unsafeBitCast(fn, to: Fn.self)
}
