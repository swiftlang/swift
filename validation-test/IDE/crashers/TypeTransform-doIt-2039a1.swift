// {"kind":"complete","original":"747a5bcd","signature":"swift::TypeTransform<(anonymous namespace)::TypeSimplifier>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (!ty->is<InOutType>() && \"Cannot have InOutType in a tuple\"), function TupleTypeElt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
( [Int:Int ]) {
a, b in b= b < b
}#^^#
