// {"kind":"complete","original":"3f6f8010","signature":"swift::ConformanceChecker::resolveWitnessViaDerivation(swift::ValueDecl*)","signatureAssert":"Assertion failed: (Conformance->getWitnessUncached(requirement).getDecl() == match.Witness && \"Deduced different witnesses?\"), function recordWitness","signatureNext":"ConformanceChecker::resolveWitnessTryingAllStrategies"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a: Codable {
  func
    encode(to: Encoder) throws
  {
    {
      encode
      #^^#
    }
  }
  func
    encode(to: Encoder) throws
}
