// {"kind":"typecheck","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)","signatureAssert":"Assertion failed: (isAdmissibleType(First)), function Constraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension _UIntBuffer {
  a {
    struct b : OptionSet
