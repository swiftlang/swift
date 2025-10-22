// {"kind":"typecheck","original":"06207482","signature":"swift::InferredGenericSignatureRequest::evaluate(swift::Evaluator&, swift::GenericSignatureImpl const*, swift::GenericParamList*, swift::WhereClauseOwner, llvm::SmallVector<swift::Requirement, 2u>, llvm::SmallVector<swift::TypeBase*, 2u>, swift::SourceLoc, swift::ExtensionDecl*, bool) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: %target-typecheck-verify-swift
@available(SwiftStdlib 6.2, *)
class a<let b: b> {
  // expected-error@-1 {{circular reference}}
  // expected-note@-2 {{through reference here}}
  // expected-note@-3 {{while resolving type 'b'}}
}
