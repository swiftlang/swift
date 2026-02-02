// {"kind":"typecheck","original":"16ab065c","signature":"swift::rewriting::RequirementMachine::checkCompletionResult(swift::rewriting::CompletionResult) const"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a: b
  protocol b {
    associatedtype c
  }
  protocol d: e,
    f
    protocol f: b where c: f
      protocol e: a where c: e
