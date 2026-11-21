// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_opened_existential %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import StdlibUnittest

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

// CHECK-LABEL: // reverse-mode derivative of openedExistentialBranch(_:_:_:)
// CHECK:         open_existential_addr immutable_access
// CHECK:         // function_ref closure #1 in makeOpenedPullback<A>(_:)
// CHECK-NEXT:    %[[#ROOT:]] = function_ref
// CHECK-NEXT:    %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]]<@opened({{.*}}) Self>()
// CHECK-NEXT:    %[[#PAYLOAD:]] = tuple $(predecessor: {{.*}}, @callee_guaranteed (Float) -> Float) (%[[#]], %[[#CALLBACK]])
// CHECK-NEXT:    %[[#]] = enum $_AD__{{[^,]*_spec[^,]*}}, {{.*}}, %[[#PAYLOAD]]
// CHECK-LABEL: // specialized pullback of openedExistentialBranch(_:_:_:)
// CHECK:       bb[[#]](%[[#]] : $(predecessor: {{[^,]+}}, (Float, Float))):
// CHECK:       bb[[#]](%[[#SAVED:]] : $(predecessor: {{[^,]+}}, @callee_guaranteed (Float) -> Float)):
// CHECK-NEXT:    %[[#STORED:]] = tuple_extract %[[#SAVED]], 1
// CHECK-NEXT:    %[[#]] = apply %[[#STORED]](%[[#]]) : $@callee_guaranteed (Float) -> Float
// CHECK:       } // end sil function

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
