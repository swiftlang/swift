// {"kind":"typecheck","original":"8ab83716","signature":"swift::TypeBase::isEqual(swift::Type) const","signatureNext":"CompareDeclSpecializationRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  func b(Int)
  struct c < d : a, e where e.f == Int, d == e.f {
b
    let g = b
