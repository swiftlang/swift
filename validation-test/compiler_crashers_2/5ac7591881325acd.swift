// {"signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension _UIntBuffer {
  a {
    struct b : OptionSet
