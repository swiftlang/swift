// {"kind":"typecheck","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureAssert":"Assertion failed: (Context.SourceMgr.hasIDEInspectionTargetBuffer() || Context.LangOpts.IsForSourceKit || Context.TypeCheckerOpts.EnableLazyTypecheck || inSecondaryScriptFile() && \"Querying VarDecl's type before type-checking parent stmt\"), function evaluate","signatureNext":"NamingPatternRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  func b() {}
}
_ = { c }
var d: a?
guard let d else {}
let c = d.b()
