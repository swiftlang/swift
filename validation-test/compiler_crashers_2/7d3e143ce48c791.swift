// {"signature":"swift::SourceFile::getIfConfigClausesWithin(swift::SourceRange) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  if
  case.(let \ a) { a
