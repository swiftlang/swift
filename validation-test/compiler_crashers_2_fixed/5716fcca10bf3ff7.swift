// {"signature":"createImplicitConstructor(swift::NominalTypeDecl*, ImplicitConstructorKind, swift::ASTContext&)"}
// RUN: not %target-swift-frontend -typecheck -swift-version 5 %s
struct a @propertyWrapper struct b < c {
  wrappedValue : c
} @propertyWrapper struct d {
  var wrappedValue @b var e : a @d var f
