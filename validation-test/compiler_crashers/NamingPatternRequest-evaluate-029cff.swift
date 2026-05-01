// {"kind":"typecheck","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureAssert":"Assertion failed: (Context.SourceMgr.hasIDEInspectionTargetBuffer() || Context.LangOpts.IsForSourceKit || Context.TypeCheckerOpts.EnableLazyTypecheck && \"Querying VarDecl's type before type-checking parent stmt\"), function evaluate","signatureNext":"NamingPatternRequest::OutputType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a guard let b let a = b
