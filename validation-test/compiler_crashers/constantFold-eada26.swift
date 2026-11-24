// {"kind":"emit-sil","signature":"(anonymous namespace)::constantFold(swift::SILInstruction*, swift::SingleValueInstruction*, unsigned int)","signatureAssert":"Assertion failed: (!endUsersOfOSLogMessage.empty()), function constantFold","signatureNext":"OSLogOptimization::run"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
struct a: ExpressibleByStringInterpolation {
  @_semantics("oslog.message.init_stringliteral") init(stringLiteral: String)
}
func b(c: a = "") {
}
