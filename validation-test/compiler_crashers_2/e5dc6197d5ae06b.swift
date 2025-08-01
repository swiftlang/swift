// {"kind":"typecheck","signature":"swift::Mangle::ASTMangler::appendExistentialLayout(swift::ExistentialLayout const&, swift::GenericSignature, swift::ValueDecl const*)"}
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
