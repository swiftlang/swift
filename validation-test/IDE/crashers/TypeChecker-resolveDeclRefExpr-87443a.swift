// {"kind":"complete","original":"bf3927a1","signature":"swift::TypeChecker::resolveDeclRefExpr(swift::UnresolvedDeclRefExpr*, swift::DeclContext*)","signatureAssert":"Assertion failed: (isValid() && \"Can't advance an invalid location\"), function getAdvancedLoc"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
postfix operator %%
postfix func % (a)
func b c b #^^#
