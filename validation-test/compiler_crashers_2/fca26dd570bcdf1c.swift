// {"signature":"(anonymous namespace)::TypeResolver::resolveVarargType(swift::VarargTypeRepr*, swift::TypeResolutionOptions)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : Int...
