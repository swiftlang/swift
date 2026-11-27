// {"frontendArgs":["-emit-silgen","-module-name","main","-parse-as-library"],"kind":"custom","signature":"swift::SILInstruction::verifyOperandOwnership(swift::SILAddressConventions*) const","signatureNext":"SILBuilder::insertImpl"}
// RUN: not --crash %target-swift-frontend -emit-silgen -module-name main -parse-as-library %s
@main
struct S {
  static func main() throws(DecodingError) {
  }
}
