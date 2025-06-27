// {"signature":"(anonymous namespace)::TypeResolver::resolveType(swift::TypeRepr*, swift::TypeResolutionOptions)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a open extension a {
  @objc b : _
