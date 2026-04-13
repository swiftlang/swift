// {"kind":"complete","original":"5bc9ffed","signature":"(anonymous namespace)::TypePrinter::printWithParensIfNotSimple(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypePrinter::visitAnyMetatypeType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
import Foundation
var a {
  #^^#
}
func b() -> (some Formatter).Type
