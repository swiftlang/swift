// {"signature":"createImplicitConstructor(swift::NominalTypeDecl*, ImplicitConstructorKind, swift::ASTContext&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a @propertyWrapper struct b < c {
  wrappedValue : c
} @propertyWrapper struct d {
  var wrappedValue @b var e : a @d var f
