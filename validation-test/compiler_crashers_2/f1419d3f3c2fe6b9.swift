// {"signature":"(anonymous namespace)::TypeSubstituter::transformGenericTypeParamType(swift::GenericTypeParamType*, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Collection where Self : a {
  struct Index protocol a
