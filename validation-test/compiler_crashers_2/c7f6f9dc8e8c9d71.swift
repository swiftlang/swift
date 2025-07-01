// {"signature":"swift::constraints::ConstraintSystem::setBuilderTransformedBody(swift::AnyFunctionRef, swift::NominalTypeDecl*, swift::NullablePtr<swift::VarDecl>, swift::NullablePtr<swift::BraceStmt>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  @resultBuilder struct b
  }
  struct c < d {
    init(@b content : () -> d
  }
  extension c : a
    struct b {
e {
  c {
