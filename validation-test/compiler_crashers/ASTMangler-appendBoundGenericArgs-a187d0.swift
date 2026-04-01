// {"kind":"typecheck","original":"4e63dcdd","signature":"swift::Mangle::ASTMangler::appendBoundGenericArgs(swift::Type, swift::GenericSignature, bool&, swift::ValueDecl const*)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b -> Collection<some a>[]
