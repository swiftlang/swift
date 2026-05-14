// {"kind":"typecheck","original":"863a4d82","signature":"swift::DerivedConformance::deriveDecodable(swift::ValueDecl*)","signatureNext":"ConformanceChecker::resolveWitnessViaDerivation"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b>: Codable {
  typealias CodingKeys = b
}
