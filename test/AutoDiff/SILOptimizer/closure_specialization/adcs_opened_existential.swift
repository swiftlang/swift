// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_opened_existential %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

public protocol PullbackMarker {
  static var identifier: Int { get }
}
public struct FirstMarker: PullbackMarker {
  public static var identifier: Int { 1 }
}
public struct SecondMarker: PullbackMarker {
  public static var identifier: Int { 2 }
}

public var observedPullbackType: Any.Type? = nil
public var observedPullbackIdentifier: Int? = nil

@inline(never)
public func recordPullbackType<T: PullbackMarker>(_ type: T.Type) {
  observedPullbackType = type
  observedPullbackIdentifier = T.identifier
}

// Even with no explicit value captures, the closure captures the opened
// existential's type metadata and conformance.
@inline(__always)
public func makeOpenedPullback<T: PullbackMarker>(_ marker: T) -> (Float) -> Float {
  { seed in
    recordPullbackType(T.self)
    return seed
  }
}

@differentiable(reverse, wrt: x)
public func markedIdentity(_ x: Float, _ marker: any PullbackMarker) -> Float {
  x
}

@derivative(of: markedIdentity, wrt: x)
@inline(__always)
public func vjpMarkedIdentity(_ x: Float, _ marker: any PullbackMarker)
  -> (value: Float, pullback: (Float) -> Float) {
  (x, makeOpenedPullback(marker))
}

@differentiable(reverse, wrt: x)
public func openedExistentialBranch(
  _ x: Float, _ marker: any PullbackMarker, _ flag: Bool
) -> Float {
  if flag { return markedIdentity(x, marker) }
  return x * x
}

// CHECK:      enum [[TAPE:_AD__[^ ]+_bb3__Pred__src_0_wrt_0_spec[^ ]+]] {
// CHECK-NEXT: case bb2((predecessor: {{.*}}, (Float, Float)))
// CHECK-NOT:  }
// CHECK:      case bb1((predecessor: {{.*}}, @callee_guaranteed (Float) -> Float))

// CHECK-LABEL: {{^}}// reverse-mode derivative of openedExistentialBranch(_:_:_:)
// CHECK:       open_existential_addr
// CHECK:         // function_ref closure #1 in makeOpenedPullback<A>(_:)
// CHECK-NEXT:    %[[#ROOT:]] = function_ref
// CHECK-NEXT:    %[[#CALLBACK:]] = partial_apply {{.*}} %[[#ROOT]]<@opened({{.*}}) Self>()
// CHECK:       tuple ${{.*}}@callee_guaranteed (Float) -> Float) ({{.*}}%[[#CALLBACK]])
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK:         apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK:       } // end sil function
