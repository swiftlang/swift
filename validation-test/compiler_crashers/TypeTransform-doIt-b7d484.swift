// {"kind":"typecheck","signature":"swift::TypeTransform<(anonymous namespace)::TypeSimplifier>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (!ty->is<InOutType>() && \"Cannot have InOutType in a tuple\"), function TupleTypeElt","signatureNext":"ConstraintSystem::simplifyType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
[Int : Int](Int) { a, b in a[b b= b * b
