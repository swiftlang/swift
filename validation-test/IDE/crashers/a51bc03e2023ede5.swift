// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::buildDisjunctionForOptionalVsUnderlying(swift::Type, swift::Type, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {b: c! = b #^COMPLETE^#
