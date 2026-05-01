// {"kind":"typecheck","original":"132f5134","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Context.SourceMgr.hasIDEInspectionTargetBuffer() || Context.LangOpts.IsForSourceKit || Context.TypeCheckerOpts.EnableLazyTypecheck && \"Querying VarDecl's type before type-checking parent stmt\"), function evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension {
a { for
b {
defer {
b
