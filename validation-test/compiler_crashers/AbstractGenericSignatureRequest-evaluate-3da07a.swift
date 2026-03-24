// {"kind":"typecheck","original":"1acdb58d","signature":"swift::AbstractGenericSignatureRequest::evaluate(swift::Evaluator&, swift::GenericSignatureImpl const*, llvm::SmallVector<swift::GenericTypeParamType*, 2u>, llvm::SmallVector<swift::Requirement, 2u>, bool) const","signatureNext":"AbstractGenericSignatureRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : c
}
protocol c {
  associatedtype d : a
  associatedtype e
  : a
  func
    f< g : c where g.d ==
    e>()
  struct h< i >
  : c {
    typealias e =
    j< i >
    func f()
    struct j< k >
    : a {
      typealias b = h
    }
  }
}
