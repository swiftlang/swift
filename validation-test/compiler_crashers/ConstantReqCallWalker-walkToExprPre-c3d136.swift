// {"kind":"typecheck","signature":"swift::diagnoseConstantArgumentRequirement(swift::Expr const*, swift::DeclContext const*)::ConstantReqCallWalker::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (constantIndex < arguments.size() && \"constantIndex exceeds the number of arguments to the function\"), function diagnoseConstantArgumentRequirementOfCall"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @_semantics("oslog.requires_constant_arguments") func a(b) { a(
