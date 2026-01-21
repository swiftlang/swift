// {"kind":"typecheck","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureAssert":"Assertion failed: (Context.SourceMgr.hasIDEInspectionTargetBuffer() || Context.TypeCheckerOpts.EnableLazyTypecheck && \"Querying VarDecl's type before type-checking parent stmt\"), function evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  func b() {}
}
_ = { c }
var d: a?
guard let d else {}
let c = d.b()
