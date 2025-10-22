// {"kind":"typecheck","signature":"swift::DeclAttribute::attachToDecl(swift::Decl*)","signatureAssert":"Assertion failed: (!OriginalDeclaration || OriginalDeclaration == originalDeclaration && \"Original declaration cannot have already been set\"), function attachToDeclImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@differentiable () let a, b
