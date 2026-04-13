// {"kind":"complete","original":"ca1cd711","signature":"swift::ExistentialArchetypeType::getAny(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"CompletionLookup::getValueExprCompletions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a where b == Self func c(d: a) { d#^^#
