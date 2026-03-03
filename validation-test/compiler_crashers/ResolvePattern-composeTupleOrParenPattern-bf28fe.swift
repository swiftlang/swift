// {"kind":"typecheck","signature":"(anonymous namespace)::ResolvePattern::composeTupleOrParenPattern(swift::ArgumentList*)","signatureAssert":"Assertion failed: (!args->hasAnyInOutArgs()), function composeTupleOrParenPattern"}
// RUN: not --crash %target-swift-frontend -typecheck %s
switch { case .a(&b
