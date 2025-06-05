// {"signature":"swift::ast_scope::SpecializeAttributeScope::getSourceRangeOfThisASTNode(bool) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
{
  @in
