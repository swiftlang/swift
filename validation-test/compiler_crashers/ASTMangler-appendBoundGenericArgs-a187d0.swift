// {"kind":"typecheck","original":"4e63dcdd","signature":"swift::Mangle::ASTMangler::appendBoundGenericArgs(swift::Type, swift::GenericSignature, bool&, swift::ValueDecl const*)","stackOverflow":true}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b -> Collection<some a>[]
