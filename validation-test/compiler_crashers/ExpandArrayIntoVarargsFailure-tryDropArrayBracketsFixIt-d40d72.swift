// {"kind":"typecheck","signature":"swift::constraints::ExpandArrayIntoVarargsFailure::tryDropArrayBracketsFixIt(swift::Expr const*) const","signatureAssert":"Assertion failed: (!empty()), function back"}
// RUN: not --crash %target-swift-frontend -typecheck %s
ExpressibleByDictionaryLiteral([]
