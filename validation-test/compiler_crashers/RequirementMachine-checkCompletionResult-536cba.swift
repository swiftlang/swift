// {"kind":"typecheck","original":"43a31a6a","signature":"swift::rewriting::RequirementMachine::checkCompletionResult(swift::rewriting::CompletionResult) const","signatureNext":"RewriteContext::getRequirementMachine"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
  protocol d:
  a {
    associatedtype e: Sequence where e.Element == (b, c)
    func
        f<g: d where g.b == <#type#>, g.e.Element == g.b>()
  }
}
