// {"kind":"complete","original":"251e80f9","signature":"(anonymous namespace)::TypePrinter::printWithParensIfNotSimple(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypePrinter::printParentType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b{ associatedtype b func c -> b struct d: a { e<f: a>(f) -> some a<f.b> { let g = e(d()) g.c#^^#
