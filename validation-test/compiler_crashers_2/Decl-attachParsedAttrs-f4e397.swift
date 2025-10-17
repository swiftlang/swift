// {"kind":"typecheck","signature":"swift::Decl::attachParsedAttrs(swift::DeclAttributes)","signatureAssert":"Assertion failed: (!OriginalDeclaration && \"Original declaration cannot have already been set\"), function setOriginalDeclaration"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@differentiable () let a, b
