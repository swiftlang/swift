// {"kind":"typecheck","signature":"swift::DerivedConformance::deriveDecodable(swift::ValueDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var
  b, c : Codable {
    self
  }
}
