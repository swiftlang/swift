// {"kind":"typecheck","signature":"swift::ConformanceChecker::resolveWitnessViaDerivation(swift::ValueDecl*)","signatureAssert":"Assertion failed: (Conformance->getWitnessUncached(requirement).getDecl() == match.Witness && \"Deduced different witnesses?\"), function recordWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : Codable {
  b = a(
