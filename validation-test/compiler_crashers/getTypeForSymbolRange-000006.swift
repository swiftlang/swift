// {"kind":"typecheck","original":"2886b949","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureAssert":"Assertion failed: (proto == symbol.getProtocol()), function getTypeForSymbolRange","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
  typealias d = [b: c]
}
protocol
  e
{
  associatedtype f: a
  associatedtype g: a
}
protocol h: e where f.c == g.d {
  associatedtype i: a where i.c == j
}
protocol k: h where i == f {
  struct l<m: k> {
  }
}
