// {"kind":"typecheck","signature":"swift::DerivedConformance::deriveDecodable(swift::ValueDecl*)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var
  b, c : Codable {
    self
  }
}
