// {"kind":"complete","original":"0116e8cd","signature":"swift::ConformanceChecker::resolveWitnessViaLookup(swift::ValueDecl*)","signatureAssert":"Assertion failed: (Conformance->getWitnessUncached(requirement).getDecl() == match.Witness && \"Deduced different witnesses?\"), function recordWitness"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a: OptionSet {
  static rawValue = a init(rawValue: Int ) {
    #^^#
  }
}
