// {"signature":"swift::GenericTypeParamType::GenericTypeParamType(swift::GenericTypeParamDecl*, swift::RecursiveTypeProperties)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a = () extension a {
  typealias b<c> = () extension b
