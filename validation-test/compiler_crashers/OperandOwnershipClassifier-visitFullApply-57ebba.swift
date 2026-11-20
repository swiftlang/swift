// {"extraArgs":["-module-name","main","-enable-library-evolution","-parse-as-library"],"kind":"emit-silgen","signature":"(anonymous namespace)::OperandOwnershipClassifier::visitFullApply(swift::FullApplySite)","signatureAssert":"Assertion failed: (false && \"Illegal convention for non-address types\"), function getFunctionArgOwnership","signatureNext":"Operand::getOperandOwnership"}
// RUN: not --crash %target-swift-frontend -emit-silgen -module-name main -enable-library-evolution -parse-as-library %s
public enum a: Error {
}
@main struct b {
  static func main() throws(a) {
  }
}
