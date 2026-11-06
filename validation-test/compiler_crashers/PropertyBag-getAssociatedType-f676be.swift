// {"kind":"typecheck","signature":"swift::rewriting::PropertyBag::getAssociatedType(swift::Identifier)","signatureNext":"getTypeForSymbolRange"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b : a{typealias a} protocol a : b {
  typealias a
