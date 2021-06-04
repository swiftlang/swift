// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 | %FileCheck %s
// REQUIRES: concurrency

// Declarations don't mangle global actor types.
// CHECK: @$s4test10returnsOptyxycSgxyScMYccSglF
func returnsOpt<R>(_ fn: (@MainActor () -> R)?) -> (() -> R)? {
  typealias Fn = (() -> R)?
  return unsafeBitCast(fn, to: Fn.self)
}
