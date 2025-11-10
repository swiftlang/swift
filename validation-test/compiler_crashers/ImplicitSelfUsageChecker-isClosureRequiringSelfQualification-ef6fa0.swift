// {"kind":"typecheck","signature":"(anonymous namespace)::ImplicitSelfUsageChecker::isClosureRequiringSelfQualification(swift::AbstractClosureExpr const*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a : ExpressibleByBooleanLiteral {
  c(d : Bool -> (
}
{
  a(true).c {
  b in
