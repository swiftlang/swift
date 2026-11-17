// {"kind":"complete","original":"ff6334d9","signature":"(anonymous namespace)::TypePrinter::printWithParensIfNotSimple(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypePrinter::visitOptionalType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b{ associatedtype b }
struct c<d { e -> some a<d>? }
c#^^#
