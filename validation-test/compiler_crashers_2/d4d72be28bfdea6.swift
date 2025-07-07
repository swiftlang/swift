// {"signature":"swift::constraints::ExpandArrayIntoVarargsFailure::tryDropArrayBracketsFixIt(swift::Expr const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
ExpressibleByDictionaryLiteral([]
