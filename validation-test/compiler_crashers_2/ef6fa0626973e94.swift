// {"signature":"(anonymous namespace)::ImplicitSelfUsageChecker::isClosureRequiringSelfQualification(swift::AbstractClosureExpr const*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a : ExpressibleByBooleanLiteral {
  c(d : Bool -> (
}
{
  a(true).c {
  b in
