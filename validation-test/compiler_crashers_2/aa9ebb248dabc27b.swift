// {"kind":"typecheck","signature":"swift::ConformanceChecker::resolveWitnessViaDerivation(swift::ValueDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : Codable {
  b = a(
