// {"kind":"typecheck","original":"702452ac","signature":"std::__1::__function::__func<diagnoseProtocolStubFixit(swift::ASTContext&, swift::NormalProtocolConformance*, llvm::ArrayRef<swift::ASTContext::MissingWitness>)::$_0, std::__1::allocator<diagnoseProtocolStubFixit(swift::ASTContext&, swift::NormalProtocolConformance*, llvm::ArrayRef<swift::ASTContext::MissingWitness>)::$_0>, void (swift::NormalProtocolConformance*)>::operator()(swift::NormalProtocolConformance*&&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a: b  protocol b { macro a
