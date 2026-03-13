// {"kind":"typecheck","signature":"swift::constraints::doesMemberRefApplyCurriedSelf(swift::Type, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (decl->getDeclContext()->isTypeContext() && \"Expected a member reference\"), function doesMemberRefApplyCurriedSelf"}
// RUN: not %target-swift-frontend -typecheck %s
Swift < .Int
