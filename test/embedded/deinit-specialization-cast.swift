// Metadata is emitted for the destination type of a checked cast, so the
// deinits reachable from that type's value witnesses have to be specialized.
// Here the existential is always formed from `Other`, so the cast destination is
// the only reason `Castee<Int>` would need metadata.

// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -emit-sil -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -Osize -emit-sil -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_NoncopyableCasting

// CHECK-DAG: sil_moveonlydeinit $Castee<Int> {

public protocol P: ~Copyable {
  func f()
}

public struct Castee<T>: ~Copyable, P {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  public func f() {}
  deinit { p.deallocate() }
}

public struct Other: ~Copyable, P {
  public init() {}
  public func f() {}
}

// Conditional cast: checked_cast_addr_br / checked_cast_br.
@inline(never)
func conditionally(_ e: consuming any P & ~Copyable) -> Int {
  if let c = e as? Castee<Int> {
    return c.p.pointee
  }
  return 0
}

// Unconditional cast: unconditional_checked_cast_addr.
@inline(never)
func unconditionally(_ e: consuming any P & ~Copyable) -> Int {
  let c = e as! Castee<Int>
  return c.p.pointee
}

// Both call sites pass `Other`, never `Castee`, so no existential of
// `Castee<Int>` is ever formed and the cast destination is the only thing that
// makes IRGen emit metadata for it.
public func go() -> Int {
  return conditionally(Other()) + unconditionally(Other())
}
