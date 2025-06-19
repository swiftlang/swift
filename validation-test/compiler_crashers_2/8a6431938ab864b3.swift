// {"signature":"swift::rewriting::RequirementMachine::checkCompletionResult(swift::rewriting::CompletionResult) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a : b protocol b{associatedtype c} protocol d : e,
                                                         f protocol e
    : g where c : e protocol g : b protocol f : a where c : f
