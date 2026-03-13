// {"kind":"typecheck","original":"45b18ebd","signature":"swift::rewriting::Symbol::Storage::Storage(swift::rewriting::Symbol::Kind, swift::CanType, llvm::ArrayRef<swift::rewriting::Term>)","signatureAssert":"Assertion failed: (!type->hasUnboundGenericType()), function Storage"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b, c
  enum d {
    typealias e = a
    extension a where b == d, c == b.e
