// {"kind":"typecheck","original":"45b18ebd","signature":"swift::rewriting::Symbol::Storage::Storage(swift::rewriting::Symbol::Kind, swift::CanType, llvm::ArrayRef<swift::rewriting::Term>)","signatureAssert":"Assertion failed: (!type->hasUnboundGenericType()), function Storage","signatureNext":"Symbol::forConcreteType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b, c
  enum d {
    typealias e = a
    extension a where b == d, c == b.e
