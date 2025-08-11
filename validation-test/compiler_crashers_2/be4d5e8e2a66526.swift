// {"kind":"emit-silgen","signature":"swift::Mangle::ASTMangler::appendConstrainedExistential(swift::Type, swift::GenericSignature, swift::ValueDecl const*)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a<b> {
  associatedtype b
}
struct c: a {
  typealias b = c
}
func d() -> a<some a> {
  c()
}
