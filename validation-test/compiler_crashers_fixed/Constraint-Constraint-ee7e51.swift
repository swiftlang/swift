// {"kind":"typecheck","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: (isAdmissibleType(First)), function Constraint","signatureNext":"Constraint::create"}
// RUN: not %target-swift-frontend -typecheck %s
{ (a: _) in
  func b<c>(c  = a)
  b(
