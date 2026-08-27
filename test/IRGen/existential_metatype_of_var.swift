// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -O -emit-ir %s | %FileCheck %s

// `type(of:)` does not consume its operand, so when it is applied to a mutable
// variable of a loadable existential type SILGen borrows the variable's storage
// in place and emits `existential_metatype` with an *address* operand. IRGen
// used to assume that operand was always a loaded value and would over-claim
// from the resulting explosion, tripping an assertion (Explosion::claim). It
// must instead load the existential from the address.

public protocol Widget: AnyObject { var owner: AnyObject? { get } }

// A class holding an optional class-constrained existential, exercising the
// pattern from the original report. This just needs to compile without
// crashing.
final class Container {
  private var rootWidget: (any Widget)?
  private var currentWidget: (any Widget)? {
    guard let root = rootWidget else { return nil }
    var best: any Widget = root
    best = root
    _ = "\(type(of: best))"
    return root
  }
  func use() -> (any Widget)? { currentWidget }
}

// Minimal form: `type(of:)` on a mutable class-existential variable.
// CHECK-LABEL: define {{.*}} @"$s27existential_metatype_of_var15widgetTypeOfVaryAA6Widget_pXpAaC_pF"
// CHECK: call {{.*}}@swift_getObjectType
public func widgetTypeOfVar(_ w: any Widget) -> any Widget.Type {
  var best = w
  best = w
  return type(of: best)
}

// `type(of:)` on a mutable existential-metatype variable (Metatype
// representation reached through an address).
public func metatypeTypeOfVar(_ t: any Widget.Type) -> any Widget.Type.Type {
  var best = t
  best = t
  return type(of: best)
}

// `type(of:)` on a mutable boxed (error) existential variable (Boxed
// representation reached through an address).
public func errorTypeOfVar(_ e: any Error) -> any Error.Type {
  var best = e
  best = e
  return type(of: best)
}
