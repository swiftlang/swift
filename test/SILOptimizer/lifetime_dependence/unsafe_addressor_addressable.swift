// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

// rdar://189654192: A dependence on 'p.pointee' of an addressable-for-dependencies type depends on the
// UnsafeMutablePointer, not on the [unsafe] access scope of its unsafeMutableAddress accessor.

@_addressableForDependencies
struct S {
  var a: Int = 0
}

func increment(_ p: UnsafeMutablePointer<S>) {
  var ref = MutableRef(&p.pointee)
  ref.value.a += 1
}
