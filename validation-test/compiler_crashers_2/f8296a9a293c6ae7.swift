// {"kind":"typecheck","signature":"(anonymous namespace)::TypeResolver::resolveCompositionType(swift::CompositionTypeRepr*, swift::TypeResolutionOptions)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : Int &
