// {"kind":"typecheck","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: (isAdmissibleType(First)), function Constraint","signatureNext":"Constraint::create"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
extension _UIntBuffer {
  a {
    struct b : OptionSet
