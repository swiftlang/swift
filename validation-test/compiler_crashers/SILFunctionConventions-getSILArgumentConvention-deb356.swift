// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::SILFunctionConventions::getSILArgumentConvention(unsigned int) const","signatureAssert":"Assertion failed: (index < getNumSILArguments()), function getSILArgumentConvention","signatureNext":"SILBasicBlock::createFunctionArgument"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
struct a {
  var b: Int
  var c: Int {
    @storageRestrictions(initializes: b)
    init {
    }
    get {
    }
  }
}
