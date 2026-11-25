// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

var tests = TestSuite("PullbackCaptures")

// MARK: - Boxed and ordinary captures

@differentiable(reverse)
@inline(never)
public func boxedSquare(_ x: Float) -> Float { x * x }

@derivative(of: boxedSquare)
@inline(__always)
public func boxedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  var factor = 2 * x
  return (x * x, { seed in
    // The assignment keeps the capture boxed.
    factor = 2 * x
    return factor * seed
  })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return boxedSquare(x) }
  return x * x * x
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branch(_:_:)
// CHECK:             %[[#BOX:]] = alloc_box ${ var Float }
// CHECK:             // function_ref closure #1 in boxedSquareVJP(_:)
// CHECK-NEXT:        %[[#ROOT:]] = function_ref @$s3src14boxedSquareVJPySf5value_S2fc8pullbacktSfFS2fcfU_
// CHECK-NEXT:        %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]](%[[#BOX]],
// CHECK-NEXT:        %[[#BOXED_PAYLOAD:]] = tuple {{.*}}%[[#CALLBACK]])
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src6branchyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8b, #_AD__$s3src6branchyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8b.bb1!enumelt, %[[#BOXED_PAYLOAD]]
// CHECK-NOT:         partial_apply
// CHECK:             %[[#CAPTURE_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src6branchyS2f_SbtF_bb2__Pred__src_0_wrt_0, (Float, Float), (Float, Float))
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src6branchyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8b, #_AD__$s3src6branchyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8b.bb2!enumelt, %[[#CAPTURE_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of branch(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src6branchyS2f_SbtFTJpSUpSr00151_30$s3src6branchyS2f_SbtFTJrSUpSr$s3src039_AD__$s3src6branchyS2f_SbtF_bb3__Pred__A54_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8bOD_ctFCEdJyeJdTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of branch(_:_:)
// CHECK:           sil private @$s3src6branchyS2f_SbtFTJpSUpSr00151_30$s3src6branchyS2f_SbtFTJrSUpSr$s3src039_AD__$s3src6branchyS2f_SbtF_bb3__Pred__A54_0_wrt_0_spec_bb2_1_2_4a390cde657b819d06007e7eb805ce8bOD_ctFCEdJyeJdTf1nb_n
// CHECK:           bb[[#]](%[[#PAYLOAD:]] : $(predecessor: _AD__$s3src6branchyS2f_SbtF_bb1__Pred__src_0_wrt_0, @callee_guaranteed (Float) -> Float))
// CHECK-NEXT:        %[[#SAVED:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:        %[[#]] = apply %[[#SAVED]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// CHECK:           } // end sil function

tests.test("BoxedCapture") {
  for x: Float in [-3, 2] {
    for flag in [false, true] {
      let result = valueWithPullback(at: x, of: { branch($0, flag) })
      expectEqual(flag ? x * x : x * x * x, result.value)
      let derivative = flag ? 2 * x : 3 * x * x
      for seed: Float in [1, 2, -1] {
        expectEqual(derivative * seed, result.pullback(seed))
      }
    }
  }
}

// MARK: - Generic metatype capture

// The captured metatype exercises the direct call's generic substitutions.

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

tests.test("MetatypeCapture") {
  let integer = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Int.self, true)
  })
  expectEqual(6, integer.value)
  expectEqual(10, integer.pullback(5))
  expectEqual(4, integer.pullback(2))

  let floating = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Double.self, true)
  })
  expectEqual(9, floating.value)
  expectEqual(15, floating.pullback(5))

  let additive = valueWithPullback(at: Float(3), of: {
    genericBranch($0, Int.self, false)
  })
  expectEqual(9, additive.value)
  expectEqual(15, additive.pullback(5))
}

// MARK: - Shared and direct indirect captures

// Two predecessor payloads share one indirect capture allocation. Cleanup
// must deallocate it once.

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

// CHECK-LABEL:     {{^}}// reverse-mode derivative of stringShared(_:_:_:)
// CHECK:           bb0(%[[#X:]] : $Float, %[[#NAME:]] : $String,
// CHECK-NOT:         alloc_stack $String
// CHECK:             %[[#NAME_CAPTURE:]] = tuple (%[[#NAME]])
// CHECK-NEXT:        %[[#FLOAT_CAPTURES:]] = tuple (%[[#X]], %[[#X]])
// CHECK-NEXT:        %[[#SHARED:]] = tuple (%[[#NAME_CAPTURE]], %[[#FLOAT_CAPTURES]])
// CHECK:             %[[#]] = enum $_AD__$s3src12stringSharedyS2f_SSSbtF_bb1__Pred__src_0_wrt_0_spec_bb0_0_1_140d446040ff619b6a693406f94980b1, {{.*}}%[[#SHARED]]
// CHECK:             %[[#]] = enum $_AD__$s3src12stringSharedyS2f_SSSbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0_1_2e3a96ea098bd573b7fbd3facb818211, {{.*}}%[[#SHARED]]
// CHECK:             // function_ref specialized pullback of stringShared(_:_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src12stringSharedyS2f_SSSbtFTJpSUUpSr00174_40$s3src12stringSharedyS2f_SSSbtFTJrSUUpSr$s3src048_AD__$s3src12stringSharedyS2f_SSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_2ea1228bb4c4954c52cab355e462f7ebOD_ppCGEeIrfxeTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of stringShared(_:_:_:)
// CHECK:           bb[[#]](%[[#]] : $Builtin.FPIEEE32, %[[#]] : $Builtin.FPIEEE32, %[[#JOIN:]] : $((_: String), (Float, Float)))
// CHECK-NEXT:        %[[#CAPTURES:]] = tuple_extract %[[#JOIN]], 0
// CHECK:             %[[#]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func stringShared(_ x: Float, _ name: String, _ flag: Bool) -> Float {
  let y = labeled(x, Float(1), name)
  let z = x * x
  if flag { return y * z }
  return y + z
}

// Differentiating both inputs avoids the subset thunk and exercises the
// indirect String capture in a direct pullback call.
@differentiable(reverse, wrt: (x, y))
public func stringDirect(_ x: Float, _ y: Float, _ name: String, _ flag: Bool) -> Float {
  if flag { return labeled(x, y, name) }
  return x * y
}

// CHECK-LABEL:     {{^}}// specialized pullback of stringDirect(_:_:_:_:)
// CHECK:           bb[[#]](%[[#PAYLOAD:]] : $(predecessor: _AD__$s3src12stringDirectyS2f_SfSSSbtF_bb1__Pred__src_0_wrt_0_1, (_: String)))
// CHECK-NEXT:        %[[#DIRECT_CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:        %[[#]] = tuple_extract %[[#DIRECT_CAPTURES]], 0
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

tests.testWithLeakChecking("SharedIndirectString") {
  let name = String(repeating: "captured label", count: 7)
  let product = valueWithPullback(at: Float(3), of: {
    stringShared($0, name, true)
  })
  expectEqual(81, product.value)
  expectEqual(360, product.pullback(5))
  expectEqual(144, product.pullback(2))

  let sum = valueWithPullback(at: Float(3), of: {
    stringShared($0, name, false)
  })
  expectEqual(18, sum.value)
  expectEqual(40, sum.pullback(5))
}

tests.testWithLeakChecking("DirectIndirectString") {
  let name = String(repeating: "captured label", count: 7)
  let linear = valueWithPullback(at: Float(3), Float(4), of: {
    stringDirect($0, $1, name, true)
  })
  expectEqual(18, linear.value)
  expectEqual((10, 15), linear.pullback(5))
  expectEqual((4, 6), linear.pullback(2))

  let product = valueWithPullback(at: Float(3), Float(4), of: {
    stringDirect($0, $1, name, false)
  })
  expectEqual(12, product.value)
  expectEqual((20, 15), product.pullback(5))
}

// MARK: - Opened existential capture

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

// Opening the existential supplies the type metadata and conformance captured
// by the closure.
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

tests.test("ImplicitTypeCapture") {
  let markers: [(any PullbackMarker, Any.Type, Int)] = [
    (FirstMarker(), FirstMarker.self, 1),
    (SecondMarker(), SecondMarker.self, 2),
  ]
  for (marker, expectedType, expectedIdentifier) in markers {
    for flag in [true, false] {
      let result = valueWithPullback(at: Float(3), of: {
        openedExistentialBranch($0, marker, flag)
      })
      expectEqual(flag ? 3 : 9, result.value)
      for seed in [Float(1), 5, 2] {
        observedPullbackType = nil
        observedPullbackIdentifier = nil
        expectEqual(flag ? seed : 6 * seed, result.pullback(seed))
        expectTrue(flag ? observedPullbackType == expectedType : observedPullbackType == nil)
        expectEqual(flag ? expectedIdentifier : nil, observedPullbackIdentifier)
      }
    }
  }
}

// MARK: - Coroutine-produced pullbacks

public struct Accumulator: Differentiable {
  public var value: Float
  public init(_ value: Float) { self.value = value }

  public var state: Float {
    get { value }
    set { value = newValue }
    @inline(never) _modify { yield &value }
  }
}

// The accessor pullback is formed by partial_apply and invoked as a coroutine.
// CHECK:           {{^}}// specialized pullback of mutateAndBranch(_:_:)
// CHECK-LABEL:     sil private @$s3src15mutateAndBranchyS2f_SbtFTJpSUpSr00177_40$s3src15mutateAndBranchyS2f_SbtFTJrSUpSr$s3src049_AD__$s3src15mutateAndBranchyS2f_SbtF_bb3__Pred__A60_0_wrt_0_spec_bb2_1_bb1_1_2_bc2299ef5392fc644eb19af9e5bc2550OD_dyHJFeivfAeTf1nb_nTf4ng_n :
// CHECK-NOT:       } // end sil function
// CHECK:           bb[[#]]({{.*}}, %[[#PAYLOAD:]] : $((), @callee_guaranteed (@inout Accumulator.TangentVector) -> (), (Float, Float))):
// CHECK-NEXT:        %[[#TAPED:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:        %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 2
// CHECK-NEXT:        %[[#ACCESSOR:]] = convert_function %[[#TAPED]] to $@yield_once
// CHECK-NEXT:        (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESSOR]]
// CHECK-NEXT:        %[[#]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NEXT:        %[[#]] = tuple_extract %[[#CAPTURES]], 1
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             store %[[#]] to %[[#YIELD]]
// CHECK:             end_apply %[[#TOKEN]]
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func mutateAndBranch(_ x: Float, _ flag: Bool) -> Float {
  var model = Accumulator(x)
  model.state *= x
  if flag { return model.state * x }
  return model.state
}

struct Pair: Differentiable {
  var first: Float
  var second: Float
  @noDerivative var chooseFirst: Bool

  var selected: Float {
    get { chooseFirst ? first : second }
    @inline(never) _modify {
      if chooseFirst { yield &first } else { yield &second }
    }
  }
}

// With two yields, end_apply produces the accessor pullback. It shares a
// payload with the statically known multiply-assign pullback.
// CHECK:           {{^}}// specialized pullback of mutateSelected(_:_:_:)
// CHECK-LABEL:     sil private @$s3src14mutateSelectedyS2f_S2btFTJpSUUpSr00170_41$s3src14mutateSelectedyS2f_S2btFTJrSUUpSr$s3src049_AD__$s3src14mutateSelectedyS2f_S2btF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_8e661c2cbf5273a4eeb6cdb33915f443OD_FdCICewnfteTf1nb_nTf4ng_n :
// CHECK-NOT:       } // end sil function
// CHECK:           bb[[#]]({{.*}}, %[[#PAYLOAD:]] : $((), @callee_guaranteed (@inout Pair.TangentVector) -> (), (Float, Float))):
// CHECK-NEXT:        %[[#TAPED:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:        %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 2
// CHECK-NEXT:        %[[#ACCESSOR:]] = convert_function %[[#TAPED]] to $@yield_once
// CHECK-NEXT:        (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESSOR]]
// CHECK-NEXT:        %[[#]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NEXT:        %[[#]] = tuple_extract %[[#CAPTURES]], 1
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             store %[[#]] to %[[#YIELD]]
// CHECK:             end_apply %[[#TOKEN]]
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func mutateSelected(_ x: Float, _ chooseFirst: Bool, _ scale: Bool) -> Float {
  var pair = Pair(first: x, second: x, chooseFirst: chooseFirst)
  pair.selected *= x
  if scale { return pair.first * x }
  return pair.second
}

tests.test("ModifyAccessorPullback") {
  for flag in [false, true] {
    let (value, pullback) = valueWithPullback(at: Float(3), of: {
      mutateAndBranch($0, flag)
    })
    precondition(value == (flag ? 27 : 9))
    precondition(pullback(1) == (flag ? 27 : 6))
    precondition(pullback(2) == (flag ? 54 : 12))
  }
}

tests.test("ModifyAccessorWithTwoYields") {
  for (chooseFirst, scale, expected, slope): (Bool, Bool, Float, Float) in [
    (true, true, 27, 27), (true, false, 3, 1),
    (false, true, 9, 6), (false, false, 9, 6)
  ] {
    let (value, pullback) = valueWithPullback(at: Float(3)) {
      mutateSelected($0, chooseFirst, scale)
    }
    precondition(value == expected)
    precondition(pullback(1) == slope)
    precondition(pullback(2) == 2 * slope)
  }
}

runAllTests()
