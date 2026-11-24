// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::InFlightSubstitution::substType(swift::SubstitutableType*, unsigned int)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"TypeSubstituter::transformGenericTypeParamType"}
// RUN: not --crash %target-swift-frontend -emit-sil -O %s
@_specialize(where a == Int, b:_Trivial) public func c<a, b>(d: a, e: b) {
}
