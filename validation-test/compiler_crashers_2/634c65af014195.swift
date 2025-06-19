// {"signature":"swift::PackType::getSingletonPackExpansion(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a func b < each c : a {
  b
