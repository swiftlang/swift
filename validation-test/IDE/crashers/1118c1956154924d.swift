// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::assignFixedType(swift::TypeVariableType*, swift::Type, bool, bool)","signatureAssert":"Assertion failed: (!type->hasError() && \"Should not be assigning a type involving ErrorType!\"), function assignFixedType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
[ _""#^COMPLETE^#
