// REQUIRES: executable_test
// RUN: %target-run-simple-swift

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

// There are no explicit value captures. The closure still captures the type
// metadata and conformance for T when it is formed from an opened existential.
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

// Preserve the opened callback's closure context while specializing the
// independent multiplication callback to its two Float captures.
// CHECK: enum [[TAPE:_AD__[^ ]+_bb3__Pred__src_0_wrt_0_spec[^ ]+]] {
// CHECK-NEXT: case bb2((predecessor: {{.*}}, (Float, Float)))
// CHECK-NEXT: case bb1((predecessor: {{.*}}, @callee_guaranteed (Float) -> Float))

// CHECK-LABEL: // reverse-mode derivative of openedExistentialBranch(_:_:_:)
// CHECK: open_existential_addr
// CHECK: // function_ref closure #1 in makeOpenedPullback<A>(_:)
// CHECK-NEXT: %[[ROOT:[0-9]+]] = function_ref
// CHECK-NEXT: %[[CALLBACK:[0-9]+]] = partial_apply {{.*}} %[[ROOT]]<@opened({{.*}}) Self>()
// CHECK: tuple ${{.*}}@callee_guaranteed (Float) -> Float) ({{.*}}%[[CALLBACK]])
// CHECK-NOT: partial_apply
// CHECK: // function_ref specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK: } // end sil function

// CHECK-LABEL: // specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK: apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-NOT: apply {{.*}} : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK: } // end sil function

#if !DEFINITIONS_ONLY
import StdlibUnittest

var tests = TestSuite("ADCSOpenedExistential")
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
runAllTests()
#endif
