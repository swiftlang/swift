// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

// CHECK-LABEL:     {{^}}enum _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_cedd6c5ca8945aacdb3e267a0aaafbf4 {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb2__Pred__src_0_wrt_0, (Float, Float)))
// CHECK:             case bb1((predecessor: _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float))
// CHECK-LABEL:     {{^}}}

// MARK: - Shared and direct indirect captures

// CHECK-LABEL:     {{^}}// reverse-mode derivative of stringShared
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of stringShared
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src12stringSharedyS2f_SSSbtFTJpSUUpSr00174_40$s3src12stringSharedyS2f_SSSbtFTJrSUUpSr$s3src048_AD__$s3src12stringSharedyS2f_SSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_fb2174f7a7b31c2b83ee97df2316e876OD_ppCGEeIrfxeTf1nb_n :
// CHECK-NEXT:        %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:       } // end sil function
// CHECK:             return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of stringShared
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: (x, y))
public func labeled<T>(_ x: Float, _ y: Float, _ value: T) -> Float {
  2 * x + 3 * y
}

@derivative(of: labeled, wrt: (x, y))
@inline(__always)
public func vjpLabeled<T>(
  _ x: Float, _ y: Float, _ value: T
) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  (2 * x + 3 * y, { seed in withExtendedLifetime(value) { (2 * seed, 3 * seed) } })
}

// Mutually exclusive predecessor payloads share one indirect capture allocation.
// Cleanup must deallocate it only once.
@differentiable(reverse, wrt: x)
public func stringShared(_ x: Float, _ name: String, _ flag: Bool) -> Float {
  let y = labeled(x, Float(1), name)
  let z = x * x
  if flag { return y * z }
  return y + z
}

// Differentiating both inputs avoids a subset thunk and tests the direct
// call with an indirect String capture.
@differentiable(reverse, wrt: (x, y))
public func stringDirect(_ x: Float, _ y: Float, _ name: String, _ flag: Bool) -> Float {
  if flag { return labeled(x, y, name) }
  return x * y
}

// MARK: - Generic metatype captures

// CHECK-LABEL:     {{^}}// reverse-mode derivative of genericBranch<A>(_:_:_:)
// CHECK:           bb0(%[[#]] : $Float, %[[#TAG:]] : $@thick τ_0_0.Type,
// CHECK-NOT:         partial_apply
// CHECK:             %[[#CAPTURES:]] = tuple (%[[#TAG]])
// CHECK-NEXT:        %[[#PAYLOAD:]] = tuple $(predecessor: _AD__$s3src13genericBranchyS2f_xmSbtlF_bb1__Pred__src_0_wrt_0_l<τ_0_0>, (_: @thick τ_0_0.Type)) (%[[#]], %[[#CAPTURES]])
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src13genericBranchyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_2_bb1_1_e074fedcd4329c3f45b35021d296b229<τ_0_0>, #_AD__$s3src13genericBranchyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_2_bb1_1_e074fedcd4329c3f45b35021d296b229.bb1!enumelt, %[[#PAYLOAD]]
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of genericBranch<A>(_:_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src13genericBranchyS2f_xmSbtlFlTJpSUUpSr00186_43$s3src13genericBranchyS2f_xmSbtlFlTJrSUUpSr$s3src050_AD__$s3src13genericBranchyS2f_xmSbtlF_bb3__Pred__A62_0_wrt_0_l_spec_bb2_1_2_bb1_1_e074fedcd4329c3f45b35021d296b229OyxGD_cpDAAfJEfJeTf1nb_n : $@convention(thin) <τ_0_0> (Float, @owned _AD__$s3src13genericBranchyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_2_bb1_1_e074fedcd4329c3f45b35021d296b229<τ_0_0>) -> Float
// CHECK-NEXT:        %[[#BOUND:]] = partial_apply [callee_guaranteed] %[[#PB]]<τ_0_0>(%[[#]])
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#BOUND]])
// CHECK:             return %[[#RESULT]]
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of genericBranch<A>(_:_:_:)
// CHECK:           sil private @$s3src13genericBranchyS2f_xmSbtlFlTJpSUUpSr00186_43$s3src13genericBranchyS2f_xmSbtlFlTJrSUUpSr$s3src050_AD__$s3src13genericBranchyS2f_xmSbtlF_bb3__Pred__A62_0_wrt_0_l_spec_bb2_1_2_bb1_1_e074fedcd4329c3f45b35021d296b229OyxGD_cpDAAfJEfJeTf1nb_n :
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func tagged<T>(_ x: Float, _ tag: T.Type) -> Float {
  tag == Int.self ? 2 * x : 3 * x
}

@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged<T>(
  _ x: Float, _ tag: T.Type
) -> (value: Float, pullback: (Float) -> Float) {
  (tagged(x, tag), { seed in tag == Int.self ? 2 * seed : 3 * seed })
}

@differentiable(reverse, wrt: x)
public func genericBranch<T>(_ x: Float, _ tag: T.Type, _ flag: Bool) -> Float {
  if flag { return tagged(x, tag) }
  return tagged(x, tag) + x
}

// MARK: - Opened existential metadata and conformance

// CHECK-LABEL:     {{^}}// reverse-mode derivative of openedExistentialBranch(_:_:_:)
// CHECK:             open_existential_addr immutable_access
// CHECK:             // function_ref closure #1 in makeOpenedPullback<A>(_:)
// CHECK-NEXT:        %[[#ROOT:]] = function_ref @$s3src18makeOpenedPullbackyS2fcxAA0D6MarkerRzlFS2fcfU_ :
// CHECK-NEXT:        %[[#CALLBACK:]] = partial_apply {{.*}} %[[#ROOT]]<@opened({{.*}}) Self>()
// CHECK-NEXT:        %[[#PAYLOAD:]] = tuple $(predecessor: _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float) (%[[#]], %[[#CALLBACK]])
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_cedd6c5ca8945aacdb3e267a0aaafbf4, #_AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_cedd6c5ca8945aacdb3e267a0aaafbf4.bb1!enumelt, %[[#PAYLOAD]]
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK:           bb[[#]](%[[#]] : $(predecessor: _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb2__Pred__src_0_wrt_0, (Float, Float))):
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK:           bb[[#]](%[[#SAVED:]] : $(predecessor: _AD__$s3src23openedExistentialBranchyS2f_AA14PullbackMarker_pSbtF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float)):
// CHECK-NEXT:        %[[#STORED:]] = tuple_extract %[[#SAVED]], 1
// CHECK-NEXT:        %[[#]] = apply %[[#STORED]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK:           } // end sil function

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

// MARK: - Boxed captures

@differentiable(reverse)
@inline(never)
public func boxedSquare(_ x: Float) -> Float { x * x }

@derivative(of: boxedSquare)
@inline(__always)
public func boxedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  var factor = 2 * x
  return (x * x, { seed in
    factor = 2 * x
    return factor * seed
  })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return boxedSquare(x) }
  return x * x * x
}
