// {"frontendArgs":["-emit-silgen","-enable-library-evolution","-experimental-lazy-typecheck"],"kind":"custom","signature":"swift::Lowering::SILGenFunction::emitIgnoredExpr(swift::Expr*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"Lowering::SILGenTopLevel::visitTopLevelCodeDecl"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-library-evolution -experimental-lazy-typecheck %s
0
