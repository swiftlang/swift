// {"kind":"typecheck","signature":"swift::PackType::getSingletonPackExpansion(swift::Type)","signatureAssert":"Assertion failed: (rootParameterPacks.size() >= 1), function getSingletonPackExpansion"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a func b < each c : a {
  b
