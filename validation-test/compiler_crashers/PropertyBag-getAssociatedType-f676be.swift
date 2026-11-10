// {"kind":"typecheck","signature":"swift::rewriting::PropertyBag::getAssociatedType(swift::Identifier)","signatureNext":"getTypeForSymbolRange"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b : a{typealias a} protocol a : b {
  typealias a
