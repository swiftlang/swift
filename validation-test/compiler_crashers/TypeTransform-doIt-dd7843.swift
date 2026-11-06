// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (type->isTypeParameter() && \"Expected a type parameter\"), function requiresProtocol","signatureNext":"Type::transformRec"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  typealias c = a
}
struct d : e
  protocol e : a {
    associatedtype c where b == c.b
    associatedtype f = Self
