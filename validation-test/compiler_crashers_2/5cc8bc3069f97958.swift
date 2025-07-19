// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveImplicitlyUnwrappedOptionalType(swift::ImplicitlyUnwrappedOptionalTypeRepr*, swift::TypeResolutionOptions, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  enum a: b! {
  }
  guard a else {
  }
}
