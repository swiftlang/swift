// {"kind":"complete","original":"8af9696a","signature":"(anonymous namespace)::TypePrinter::printWithParensIfNotSimple(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
import Foundation
var a: some NSMutableArray = a == #^^# = <#expression#>
