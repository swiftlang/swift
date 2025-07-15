// {"kind":"typecheck","original":"34ac8b6c","signature":"swift::constraints::Constraint::Constraint(swift::FunctionType*, swift::Type, unsigned int, swift::DeclContext*, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b > {
  func
    c < let b : a >() {
      b > 0
  }
}
