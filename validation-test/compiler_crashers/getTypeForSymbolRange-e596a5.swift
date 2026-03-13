// {"kind":"typecheck","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureAssert":"Assertion failed: (std::find(conformsTo.begin(), conformsTo.end(), symbol.getProtocol()) != conformsTo.end()), function getTypeForSymbolRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  typealias b = d.b
  protocol e {
    associatedtype f
    typealias g = c<f>.b
  }
}
