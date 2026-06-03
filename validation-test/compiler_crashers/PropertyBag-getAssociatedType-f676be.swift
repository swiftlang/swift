// {"kind":"typecheck","signature":"swift::rewriting::PropertyBag::getAssociatedType(swift::Identifier)","signatureNext":"getTypeForSymbolRange"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b : a{typealias a} protocol a : b {
  typealias a
