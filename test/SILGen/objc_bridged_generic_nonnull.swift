// RUN: %target-swift-frontend -emit-silgen -verify -import-objc-header %S/Inputs/objc_bridged_generic_nonnull.h %s
// REQUIRES: objc_interop

public func test<T>(_ x: NonnullMembers<T>) -> T? {
  var z: T?
  z = x.method()
  z = x.property
  z = x.property
  z = x[x]
  _ = z
}
