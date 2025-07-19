// {"signature":"swift::diagnoseConstantArgumentRequirement(swift::Expr const*, swift::DeclContext const*)::ConstantReqCallWalker::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @_semantics("oslog.requires_constant_arguments") func a(b) { a(
