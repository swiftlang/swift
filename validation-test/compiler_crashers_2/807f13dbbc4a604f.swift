// {"signature":"swift::AccessLevelRequest::cacheResult(swift::AccessLevel) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b extension a where c == d {
  protocol d
