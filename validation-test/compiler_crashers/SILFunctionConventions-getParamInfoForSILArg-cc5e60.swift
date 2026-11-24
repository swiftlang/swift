// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::SILFunctionConventions::getParamInfoForSILArg(unsigned int) const","signatureAssert":"Assertion failed: (index >= (getNumIndirectSILResults() + getNumIndirectSILErrorResults()) && index <= getNumSILArguments()), function getParamInfoForSILArg","signatureNext":"SILFunctionArgument::getKnownParameterInfo"}
// RUN: not --crash %target-swift-frontend -emit-sil -O %s
enum a: Error {
}
@_specialize(where b == Int, c == a) public func d<b, c>(e: b, f: c) throws(c) {
}
