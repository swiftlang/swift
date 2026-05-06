// {"kind":"typecheck","original":"a4bcbc57","signature":"swift::AbstractGenericSignatureRequest::evaluate(swift::Evaluator&, swift::GenericSignatureImpl const*, llvm::SmallVector<swift::GenericTypeParamType*, 2u>, llvm::SmallVector<swift::Requirement, 2u>, bool) const","signatureNext":"AbstractGenericSignatureRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b : a
}
protocol c {
  associatedtype d : BinaryInteger
  associatedtype e : c
  func f < i
  : c where i.e ==
  Self>()
  struct g<h : a> : c {
    typealias d = Int
    typealias e = g<h.b>
    func f()
  }
}
