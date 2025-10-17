// {"kind":"typecheck","original":"5c733d48","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureAssert":"Assertion failed: (std::find(conformsTo.begin(), conformsTo.end(), symbol.getProtocol()) != conformsTo.end()), function getTypeForSymbolRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c {
  associatedtype b
  var d : b
  struct e : c {
d : some a struct f < b : a where b == e.b, g == b.b, h == b > {
      func
            i < b>()
    }
  }
}
