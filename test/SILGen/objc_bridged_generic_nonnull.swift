// RUN: %target-swift-emit-silgen -verify -import-objc-header %S/Inputs/objc_bridged_generic_nonnull.h -enable-sil-ownership %s
// REQUIRES: objc_interop

public func test<T>(_ x: NonnullMembers<T>) -> T? {
  var z: T?
  z = x.method()
  z = x.property
  z = x.property
  z = x[x]
  _ = z
}
