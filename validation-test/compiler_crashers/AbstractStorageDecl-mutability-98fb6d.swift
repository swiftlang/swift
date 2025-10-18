// {"kind":"typecheck","signature":"swift::AbstractStorageDecl::mutability(swift::DeclContext const*, std::__1::optional<swift::DeclRefExpr const*>) const","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a {
  @a $b:
