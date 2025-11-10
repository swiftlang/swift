// {"kind":"typecheck","original":"0ca20e45","signature":"(anonymous namespace)::SyntacticElementSolutionApplication::apply()","signatureAssert":"Assertion failed: (!empty()), function back"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{ a -> Void! in
} {
}
