// {"kind":"typecheck","signature":"decomposePartialApplyThunk(swift::ApplyExpr*, swift::Expr*)","signatureAssert":"Assertion failed: (false && \"Malformed curry thunk?\"), function getUnwrappedCurryThunkExpr","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b associatedtype c where c == Dictionary<String, b> func d -> c? }
                  let e: a e.d
