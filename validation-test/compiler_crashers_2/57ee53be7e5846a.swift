// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::getReducedTypeParameter(swift::CanType) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  protocol a{associatedtype b} extension a {
    extension a {
      struct c {
        d : b
