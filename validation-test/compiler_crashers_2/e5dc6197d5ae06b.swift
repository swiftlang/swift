// {"kind":"typecheck","signature":"swift::Mangle::ASTMangler::appendRequirement(swift::Requirement const&, swift::GenericSignature, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b = Int
}
protocol c {
  associatedtype b
}
extension a where b == Int
  extension a where Self : c {
    protocol Int : c {
d
