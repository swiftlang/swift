// {"kind":"complete","original":"aa09349c","signature":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a(b[c(
#^^#[a (([d: b[([d: [c(b[c(b[c(b[c(b[c( b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[.(b[c(b[c(b[.(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(e[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[c(b[.c(
