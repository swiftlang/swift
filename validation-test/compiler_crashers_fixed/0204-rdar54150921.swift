// RUN: not %target-swift-frontend -typecheck %s
{
  static func ??= (lhs: inout Optional, rhs: Optional) {}
}
