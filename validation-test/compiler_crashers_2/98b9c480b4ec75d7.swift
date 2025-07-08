// {"kind":"typecheck","signature":"swift::constraints::Solution::getFixedType(swift::TypeVariableType*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  static
b {
  return c ?? {
    return c
  }
}
c
