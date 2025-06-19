// {"signature":"swift::AbstractStorageDecl::mutability(swift::DeclContext const*, std::__1::optional<swift::DeclRefExpr const*>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  @a $b:
