// {"kind":"typecheck","signature":"swift::GenericTypeParamType::GenericTypeParamType(swift::GenericTypeParamDecl*, swift::RecursiveTypeProperties)","signatureAssert":"Assertion failed: (param->getDepth() != GenericTypeParamDecl::InvalidDepth), function GenericTypeParamType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a = () extension a {
  typealias b<c> = () extension b
