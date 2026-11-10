// {"kind":"typecheck","signature":"swift::TypeBase::removeArgumentLabels(unsigned int)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"findSimilarAccessor"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a dynamic subscript(b c: Int) a subscript(b c: Int) a @_dynamicReplacement(for: subscript) subscript(Int) a { }
