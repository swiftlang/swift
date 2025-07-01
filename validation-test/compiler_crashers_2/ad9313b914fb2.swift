// {"signature":"swift::SimpleRequest<swift::InheritedDeclsReferencedRequest, std::__1::pair<llvm::TinyPtrVector<swift::TypeDecl*>, swift::InvertibleProtocolSet> (llvm::PointerUnion<swift::TypeDecl const*, swift::ExtensionDecl const*>, unsigned int), (swift::RequestFlags)1>::noteCycleStep(swift::DiagnosticEngine&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a
  let _ b = a
  class b : b.c
